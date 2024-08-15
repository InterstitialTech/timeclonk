mod config;
mod data;
mod interfaces;
mod messages;
mod migrations;
mod sqldata;
use actix_files::NamedFile;
use actix_session::{
  config::PersistentSession, storage::CookieSessionStore, Session, SessionMiddleware,
};
use actix_web::{
  cookie::{
    self,
    time::{OffsetDateTime, UtcOffset},
    Key,
  },
  error::{ErrorInternalServerError, ErrorUnauthorized},
  http::StatusCode,
  middleware, web, App, HttpRequest, HttpResponse, HttpResponseBuilder, HttpServer, Responder,
  ResponseError,
};
use clap::Arg;
use config::Config;
use data::{InvoiceItem, PrintInvoice};
use log::{error, info};
use messages::{PublicMessage, ServerResponse, UserMessage};
use orgauth::data::WhatMessage;
use orgauth::endpoints::Callbacks;
use orgauth::util;
use serde::ser::SerializeTuple;
use serde_json;
use std::error::Error;
use std::io::stdin;
use std::path::PathBuf;
use std::process::Command;
use std::str::FromStr;
use std::{env, fmt::Display};
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
    "user msg: {}, {:?}  \n connection_info: {:?}",
    &item.what,
    &item.data,
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
    "admin msg: {}, {:?}  \n connection_info: {:?}",
    &item.what,
    &item.data,
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
    invoice_template: None,
    orgauth_config: oc,
  }
}

pub fn load_config(filename: &str) -> Result<Config, Box<dyn Error>> {
  info!("loading config: {}", filename);
  Ok(toml::from_str(&util::load_string(&filename)?)?)
}

async fn invoice(
  session: Session,
  config: web::Data<Config>,
  item: web::Json<PrintInvoice>,
  _req: HttpRequest,
) -> actix_web::Result<NamedFile> {
  info!("invoice start {:?}", item.0);
  // we logged in?  to prevent randos from making invoices
  let token = match session.get::<Uuid>("token")? {
    None => {
      return Err(ErrorUnauthorized(orgauth::error::Error::String(
        "not logged in".to_string(),
      )))
    }
    Some(t) => t,
  };

  let conn = match sqldata::connection_open(config.orgauth_config.db.as_path()) {
    Err(e) => return Err(ErrorInternalServerError(e)),
    Ok(c) => c,
  };
  let _user = match orgauth::dbfun::read_user_by_token_api(
    &conn,
    token,
    config.orgauth_config.login_token_expiration_ms,
    config.orgauth_config.regen_login_tokens,
  ) {
    Err(e) => {
      return Err(ErrorUnauthorized(e));
    }
    Ok(u) => u,
  };

  let path = run_invoice(item.0).map_err(|e| ErrorInternalServerError(e.to_string()))?; // .map_err(|e| actix_web::Error::fmt(, )

  info!("invoice here {}", path.display());
  Ok(NamedFile::open(path)?)
}

pub fn invoice_str(item: &InvoiceItem) -> String {
  format!(
    "
    (
      item: \"{}\",
      dur-min: 0,
      hours: {},
      rate: {},
    ),
    ",
    item.description, item.duration, item.rate
  )
}

pub fn run_invoice(print_invoice: PrintInvoice) -> Result<PathBuf, orgauth::error::Error> {
  let items = print_invoice
    .items
    .iter()
    .map(|item| invoice_str(item))
    .collect::<Vec<String>>()
    .concat();

  let eelines = print_invoice.payee.split('\n').count();
  let erlines = print_invoice.payer.split('\n').count();

  let (payee, payer) = match eelines.cmp(&erlines) {
    std::cmp::Ordering::Less => (
      format!(
        "{}{}",
        print_invoice.payee,
        "\n".to_string().repeat(erlines - eelines)
      ),
      print_invoice.payer,
    ),
    std::cmp::Ordering::Equal => (print_invoice.payee, print_invoice.payer),
    std::cmp::Ordering::Greater => (
      print_invoice.payee,
      format!(
        "{}{}",
        print_invoice.payer,
        "\n".to_string().repeat(eelines - erlines)
      ),
    ),
  };

  let typ = format!(
    "
#import \"./benvoice.typ\": *


#let biller = \"{}\"
#let recipient = \"{}\"


#let table-data = ( {} )

#show: invoice.with(
  language: \"en\",
  banner-image: none,
  invoice-id: \"{}\",
  // Set this to create a cancellation invoice
  // cancellation-id: \"2024-03-24t210835\",
  issuing-date: \"{}\",
  due-date: {},
  extraFields: {},
  biller: biller,
  hourly-rate: 100,
  recipient: recipient,
  tax: 0,
  items: table-data,
  styling: ( font: none ), // Explicitly use Typst's default font
)",
    payee,
    payer,
    items,
    print_invoice.id,
    print_invoice.date,
    print_invoice
      .due_date
      .map(|dd| format!("\"{}\"", dd))
      .unwrap_or("none".to_string()),
    format!(
      "( {} )",
      print_invoice
        .extra_fields
        .iter()
        .map(|ef| -> String { format!("(\"{}\", \"{}\"), ", ef.n, ef.v) })
        .collect::<Vec<String>>()
        .join("")
    ),
  );

  orgauth::util::write_string("wat.typ", typ.as_str())?;

  // return Ok("./invoice-maker/meh/en.pdf".into());
  let mut child = Command::new("typst")
    .arg("compile")
    .arg("./wat.typ")
    .spawn()
    .expect("typst failed to execute");

  match child.wait() {
    Ok(exit_code) => {
      if exit_code.success() {
        // add file to result.
        Ok("./wat.pdf".into())
      } else {
        Err(orgauth::error::Error::String(format!(
          "typst err {:?}",
          exit_code
        )))
      }
    }
    Err(e) => Err(orgauth::error::Error::String(format!(
      "invoice err {:?}",
      e
    ))),
  }
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

        println!("rd: {:?}", rd);

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
          .service(web::resource(r"/invoice").route(web::post().to(invoice)))
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
