mod config;
mod data;
mod interfaces;
mod invoice;
mod messages;
mod migrations;
mod sqldata;
use actix_session::{
  config::PersistentSession, storage::CookieSessionStore, Session, SessionMiddleware,
};
use actix_web::{
  cookie::{self, Key},
  middleware, web, App, HttpRequest, HttpResponse, HttpServer,
};
use clap::Arg;
use config::Config;
use log::{error, info};
use messages::{PublicMessage, ServerResponse, UserMessage};
use orgauth::data::WhatMessage;
use orgauth::util;
use serde_json;
use std::env;
use std::error::Error;
use std::io::stdin;
use std::path::PathBuf;
use std::str::FromStr;
use timer;
use uuid::Uuid;

/*
use actix_files::NamedFile;

TODO don't hardcode these paths.  Use config.static_path
fn favicon(_req: &HttpRequest) -> Result<NamedFile> {
  let stpath = Path::new("static/favicon.ico");
  Ok(NamedFile::open(stpath)?)
}

fn sitemap(_req: &HttpRequest) -> Result<NamedFile> {
  let stpath = Path::new("static/sitemap.txt");
  Ok(NamedFile::open(stpath)?)
}
*/

// simple index handler
async fn mainpage(session: Session, data: web::Data<Config>, req: HttpRequest) -> HttpResponse {
  info!("remote ip: {:?}, request:{:?}", req.connection_info(), req);

  // logged in?
  let logindata = match interfaces::login_data_for_token(session, &data) {
    Ok(Some(logindata)) => serde_json::to_value(logindata).unwrap_or(serde_json::Value::Null),
    _ => serde_json::Value::Null,
  };

  let adminsettings = serde_json::to_value(orgauth::data::admin_settings(&data.orgauth_config))
    .unwrap_or(serde_json::Value::Null);

  let mut staticpath = data.static_path.clone().unwrap_or(PathBuf::from("static/"));
  staticpath.push("index.html");
  match staticpath.to_str() {
    Some(path) => match orgauth::util::load_string(path) {
      Ok(s) => {
        // search and replace with logindata!
        HttpResponse::Ok()
          .content_type("text/html; charset=utf-8")
          .body(
            s.replace("{{logindata}}", logindata.to_string().as_str())
              .replace(
                "{{appname}}",
                data.orgauth_config.appname.to_string().as_str(),
              )
              .replace("{{adminsettings}}", adminsettings.to_string().as_str()),
          )
      }
      Err(e) => HttpResponse::from_error(actix_web::error::ErrorInternalServerError(e)),
    },
    None => HttpResponse::from_error(actix_web::error::ErrorInternalServerError(
      "bad static path",
    )),
  }
}

async fn public(
  data: web::Data<Config>,
  item: web::Json<PublicMessage>,
  req: HttpRequest,
) -> HttpResponse {
  info!(
    "public msg: {:?} \n connection_info: {:?}",
    &item,
    req.connection_info()
  );

  match interfaces::public_interface(&data, item.into_inner()) {
    Ok(sr) => HttpResponse::Ok().json(sr),
    Err(e) => {
      error!("'public' err: {:?}", e);
      let se = ServerResponse {
        what: "server error".to_string(),
        content: serde_json::Value::String(e.to_string()),
      };
      HttpResponse::Ok().json(se)
    }
  }
}

async fn user(
  session: Session,
  data: web::Data<Config>,
  item: web::Json<WhatMessage>,
  req: HttpRequest,
) -> HttpResponse {
  info!(
    "user msg: {}  \n connection_info: {:?}",
    &item.what,
    req.connection_info()
  );
  let mut cb = sqldata::timeclonk_callbacks();

  match orgauth::endpoints::user_interface(
    &session,
    &data.orgauth_config,
    &mut cb,
    item.into_inner(),
  ) {
    Ok(sr) => HttpResponse::Ok().json(sr),
    Err(e) => {
      error!("'user' err: {:?}", e);
      let se = orgauth::data::WhatMessage {
        what: "server error".to_string(),
        data: Some(serde_json::Value::String(e.to_string())),
      };
      HttpResponse::Ok().json(se)
    }
  }
}

async fn admin(
  session: Session,
  data: web::Data<Config>,
  item: web::Json<orgauth::data::WhatMessage>,
  req: HttpRequest,
) -> HttpResponse {
  info!(
    "admin msg: {}  \n connection_info: {:?}",
    &item.what,
    req.connection_info()
  );
  let mut cb = sqldata::timeclonk_callbacks();
  match orgauth::endpoints::admin_interface_check(
    &session,
    &data.orgauth_config,
    &mut cb,
    item.into_inner(),
  ) {
    Ok(sr) => HttpResponse::Ok().json(sr),
    Err(e) => {
      error!("'user' err: {:?}", e);
      let se = orgauth::data::WhatMessage {
        what: "server error".to_string(),
        data: Some(serde_json::Value::String(e.to_string())),
      };
      HttpResponse::Ok().json(se)
    }
  }
}

async fn private(
  session: Session,
  data: web::Data<Config>,
  item: web::Json<UserMessage>,
  _req: HttpRequest,
) -> HttpResponse {
  match timeclonk_interface_check(&session, &data, item.into_inner()) {
    Ok(sr) => HttpResponse::Ok().json(sr),
    Err(e) => {
      error!("'private' err: {:?}", e);
      let se = ServerResponse {
        what: "server error".to_string(),
        content: serde_json::Value::String(e.to_string()),
      };
      HttpResponse::Ok().json(se)
    }
  }
}

fn timeclonk_interface_check(
  session: &Session,
  config: &Config,
  msg: UserMessage,
) -> Result<ServerResponse, Box<dyn Error>> {
  match session.get::<Uuid>("token")? {
    None => Ok(ServerResponse {
      what: "not logged in".to_string(),
      content: serde_json::Value::Null,
    }),
    Some(token) => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      match orgauth::dbfun::read_user_by_token_api(
        &conn,
        token,
        config.orgauth_config.login_token_expiration_ms,
        config.orgauth_config.regen_login_tokens,
      ) {
        Err(e) => {
          info!("read_user_by_token_api error: {:?}", e);

          Ok(ServerResponse {
            what: "invalid user or pwd".to_string(),
            content: serde_json::Value::Null,
          })
        }
        Ok(userdata) => {
          // finally!  processing messages as logged in user.
          interfaces::timeclonk_interface_loggedin(&config, userdata.id, &msg)
        }
      }
    }
  }
}

async fn register(data: web::Data<Config>, req: HttpRequest) -> HttpResponse {
  orgauth::endpoints::register(&data.orgauth_config, req)
}

async fn new_email(data: web::Data<Config>, req: HttpRequest) -> HttpResponse {
  orgauth::endpoints::new_email(&data.orgauth_config, req)
}

fn defcon() -> Config {
  let oc = orgauth::data::Config {
    db: PathBuf::from("./timeclonk.db"),
    mainsite: "http://localhost:8000".to_string(),
    appname: "timeclonk".to_string(),
    emaildomain: "localhost:8000".to_string(),
    admin_email: "admin@admin.admin".to_string(),
    regen_login_tokens: false,
    login_token_expiration_ms: Some(7 * 24 * 60 * 60 * 1000), // 7 days in milliseconds
    email_token_expiration_ms: 1 * 24 * 60 * 60 * 1000,       // 1 day in milliseconds
    reset_token_expiration_ms: 1 * 24 * 60 * 60 * 1000,       // 1 day in milliseconds
    invite_token_expiration_ms: 1 * 24 * 60 * 60 * 1000,      // 1 day in milliseconds
    open_registration: false,
    non_admin_invite: false,
  };
  Config {
    ip: "127.0.0.1".to_string(),
    port: 8000,
    static_path: None,
    orgauth_config: oc,
  }
}

pub fn load_config(filename: &str) -> Result<Config, Box<dyn Error>> {
  info!("loading config: {}", filename);
  Ok(toml::from_str(&util::load_string(&filename)?)?)
}

fn main() {
  match err_main() {
    Err(e) => error!("error: {:?}", e),
    Ok(_) => (),
  }
}

#[actix_web::main]
async fn err_main() -> Result<(), Box<dyn Error>> {
  let matches = clap::App::new("timeclonk server")
    .version("1.0")
    .author("Ben Burdette")
    .about("team time clock web server")
    // .arg(
    //   Arg::with_name("export")
    //     .short("e")
    //     .long("export")
    //     .value_name("FILE")
    //     .help("Export database to json")
    //     .takes_value(true),
    // )
    .arg(
      Arg::with_name("config")
        .short("c")
        .long("config")
        .value_name("FILE")
        .help("specify config file")
        .takes_value(true),
    )
    .arg(
      Arg::with_name("write_config")
        .short("w")
        .long("write_config")
        .value_name("FILE")
        .help("write default config file")
        .takes_value(true),
    )
    .arg(
      Arg::with_name("promote_to_admin")
        .short("p")
        .long("promote_to_admin")
        .value_name("user name")
        .help("grant admin privileges to user")
        .takes_value(true),
    )
    .arg(
      Arg::with_name("create_admin_user")
        .short("a")
        .long("create_admin_user")
        .value_name("user name")
        .help("create new admin user")
        .takes_value(true),
    )
    .get_matches();

  // writing a config file?
  if let Some(filename) = matches.value_of("write_config") {
    util::write_string(filename, toml::to_string_pretty(&defcon())?.as_str())?;
    info!("default config written to file: {}", filename);
    return Ok(());
  }

  // specifying a config file?  otherwise try to load the default.
  let mut config = match matches.value_of("config") {
    Some(filename) => load_config(filename)?,
    None => load_config("config.toml")?,
  };

  // are we exporting the DB?
  match matches.value_of("export") {
    Some(_exportfile) => {
      // do that exporting...
      sqldata::dbinit(
        config.orgauth_config.db.as_path(),
        config.orgauth_config.login_token_expiration_ms,
      )?;

      error!("export is unimplemented!");

      // util::write_string(
      //   exportfile,
      //   serde_json::to_string_pretty(&sqldata::export_db(config.db.as_path())?)?.as_str(),
      // )?;

      Ok(())
    }
    None => {
      // normal server ops
      env_logger::init();

      info!("server init!");

      if config.static_path == None {
        for (key, value) in env::vars() {
          if key == "TIMECLONK_STATIC_PATH" {
            config.static_path = PathBuf::from_str(value.as_str()).ok();
          }
        }
      }

      let id = PathBuf::from(invoice::INVOICE_DIR); // .to_string().into()?;
      if !std::path::Path::exists(&id) {
        std::fs::create_dir_all(&id)?;
      }

      info!("config: {:?}", config);

      sqldata::dbinit(
        config.orgauth_config.db.as_path(),
        config.orgauth_config.login_token_expiration_ms,
      )?;

      let timer = timer::Timer::new();

      let ptconfig = config.clone();

      let _guard = timer.schedule_repeating(chrono::Duration::days(1), move || {
        match orgauth::dbfun::purge_tokens(&ptconfig.orgauth_config) {
          Err(e) => error!("purge_login_tokens error: {}", e),
          Ok(_) => (),
        }
      });

      // promoting a user to admin?
      if let Some(uid) = matches.value_of("promote_to_admin") {
        let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
        let mut user = orgauth::dbfun::read_user_by_name(&conn, uid)?;
        user.admin = true;
        orgauth::dbfun::update_user(&conn, &user)?;

        info!("promoted user {} to admin", uid);
        return Ok(());
      }

      // creating an admin user?
      if let Some(username) = matches.value_of("create_admin_user") {
        // prompt for password.
        println!("Enter password for admin user '{}':", username);
        let mut pwd = String::new();
        stdin().read_line(&mut pwd)?;
        let mut cb = sqldata::timeclonk_callbacks();

        let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
        // make new registration i
        let rd = orgauth::data::RegistrationData {
          uid: username.to_string(),
          pwd: pwd.trim().to_string(),
          email: "".to_string(),
        };

        orgauth::dbfun::new_user(&conn, &rd, None, None, true, None, &mut cb.on_new_user)?;

        println!("admin user created: {}", username);
        return Ok(());
      }

      let c = config.clone();
      HttpServer::new(move || {
        let staticpath = c.static_path.clone().unwrap_or(PathBuf::from("static/"));
        App::new()
          .app_data(web::Data::new(c.clone())) // <- create app with shared state
          .wrap(middleware::Logger::default())
          .wrap(
            SessionMiddleware::builder(CookieSessionStore::default(), Key::from(&[0; 64]))
              .cookie_secure(false)
              // one year cookie duration
              .session_lifecycle(
                PersistentSession::default().session_ttl(cookie::time::Duration::weeks(52)),
              )
              .build(),
          )
          .service(web::resource("/public").route(web::post().to(public)))
          .service(web::resource("/private").route(web::post().to(private)))
          .service(web::resource("/user").route(web::post().to(user)))
          .service(web::resource("/admin").route(web::post().to(admin)))
          .service(web::resource(r"/register/{uid}/{key}").route(web::get().to(register)))
          .service(web::resource(r"/newemail/{uid}/{token}").route(web::get().to(new_email)))
          .service(web::resource(r"/invoice").route(web::post().to(invoice::invoice)))
          .service(actix_files::Files::new("/static/", staticpath))
          .service(web::resource("/{tail:.*}").route(web::get().to(mainpage)))
      })
      .bind(format!("{}:{}", config.ip, config.port))?
      .run()
      .await?;

      Ok(())
    }
  }
}
