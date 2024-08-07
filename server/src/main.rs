mod config;
mod data;
mod interfaces;
mod messages;
mod migrations;
mod sqldata;
use actix_session::{
  config::PersistentSession, storage::CookieSessionStore, Session, SessionMiddleware,
};
use actix_web::{
  cookie::{self, Key},
  middleware, web, App, HttpRequest, HttpResponse, HttpServer, Result,
};
use clap::Arg;
use config::Config;
use log::{error, info};
use messages::{PublicMessage, ServerResponse, UserMessage};
use orgauth::data::WhatMessage;
use orgauth::endpoints::Callbacks;
use serde_json;
use std::env;
use std::error::Error;
use std::path::PathBuf;
use std::str::FromStr;
use timer;
use uuid::Uuid;
use typst;
use data::PrintInvoice;

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
  let mut cb = Callbacks {
    on_new_user: Box::new(sqldata::on_new_user),
    on_delete_user: Box::new(sqldata::on_delete_user),
    extra_login_data: Box::new(sqldata::extra_login_data_callback),
  };

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
  let mut cb = Callbacks {
    on_new_user: Box::new(sqldata::on_new_user),
    extra_login_data: Box::new(sqldata::extra_login_data_callback),
    on_delete_user: Box::new(sqldata::on_delete_user),
  };
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
    mainsite: "http://localhost:8001".to_string(),
    appname: "timeclonk".to_string(),
    emaildomain: "localhost:8001".to_string(),
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

fn load_config() -> Config {
  match orgauth::util::load_string("config.toml") {
    Err(e) => {
      error!("error loading config.toml: {:?}", e);
      defcon()
    }
    Ok(config_str) => match toml::from_str(config_str.as_str()) {
      Ok(c) => c,
      Err(e) => {
        error!("error loading config.toml: {:?}", e);
        defcon()
      }
    },
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
    .arg(
      Arg::with_name("export")
        .short("e")
        .long("export")
        .value_name("FILE")
        .help("Export database to json")
        .takes_value(true),
    )
    .get_matches();

  // are we exporting the DB?
  match matches.value_of("export") {
    Some(_exportfile) => {
      // do that exporting...
      let config = load_config();

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

      let mut config = load_config();

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
async fn invoice(session: Session, config: web::Data<Config>, 
  item: web::Json<PrintInvoice>,
  req: HttpRequest) -> HttpResponse {

  typst::world::SystemWorld

  typst::compile("blah");

  // let conn = match sqldata::connection_open(config.orgauth_config.db.as_path()) {
  //   Ok(c) => c,
  //   Err(e) => return HttpResponse::InternalServerError().body(format!("{:?}", e)),
  // };

  // let suser = match session_user(&conn, session, &config) {
  //   Ok(Either::Left(user)) => Some(user),
  //   Ok(Either::Right(_sr)) => None,
  //   Err(e) => return HttpResponse::InternalServerError().body(format!("{:?}", e)),
  // };

  // let uid = suser.map(|user| user.id);

  // match req
  //   .match_info()
  //   .get("id")
  //   // .and_then(|s| s.parse::<i64>().ok())
  // {
  //   Some(noteid) => {
  //     let uuid = match Uuid::parse_str(noteid) {
  //       Ok(id) => id,
  //       Err(e) => return HttpResponse::BadRequest().body(e.to_string())
  //      };
  //     let nid = match sqldata::note_id_for_uuid(&conn, &uuid) {
  //       Ok(id) => id,
  //       Err(e) => return HttpResponse::NotFound().body(e.to_string())
  //      };
  //       let hash = match sqldata::read_zknote_filehash(&conn, uid, nid) {
  //       Ok(Some(hash)) => hash,
  //       Ok(None) => return HttpResponse::NotFound().body("not found"),
  //       Err(e) => return HttpResponse::InternalServerError().body(format!("{:?}", e)),
  //     };

  //     let zkln = match sqldata::read_zklistnote(&conn, &config.file_path, uid, nid) {
  //       Ok(zkln) => zkln,
  //       Err(e) => return HttpResponse::InternalServerError().body(format!("{:?}", e)),
  //     };

  //     let stpath = config.file_path.join(hash);

  //     match File::open(stpath).and_then(|f| NamedFile::from_file(f, Path::new(zkln.title.as_str())))
  //     {
  //       Ok(f) => f.into_response(&req),
  //       Err(e) => HttpResponse::NotFound().body(format!("{:?}", e)),
  //     }
  //   }
  //   None => HttpResponse::BadRequest().body("file id required: /file/<id>"),
  // }
  HttpResponse::Ok().body("")
}

pub fn invoice(
  conn: &Connection,
  uid: i64,
  savedir: &Path,
  printInvoice: PrintInvoice,
) -> Result<PathBuf, orgauth::error::Error> {

  

  let mut child = Command::new("typst")
    .arg("compile")
    .arg(format!("-o{}/%(title)s-%(id)s.%(ext)s", savedir.display()))
    .arg(yeet.url.clone())
    .spawn()
    .expect("youtube-dl failed to execute");

  match child.wait() {
    Ok(exit_code) => {
      if exit_code.success() {
        // find the yeeted file by 'v'.
        let file: PathBuf = match glob::glob(format!("{}/*{}*", savedir.display(), hv.v).as_str()) {
          Ok(mut paths) => match paths.next() {
            Some(rpb) => match rpb {
              Ok(pb) => pb,
              Err(e) => return Err(orgauth::error::Error::String(format!("glob error {:?}", e))),
            },
            None => {
              return Err(orgauth::error::Error::String(format!(
                "yeet file not found {:?}",
                hv.v
              )))
            }
          },
          Err(e) => return Err(orgauth::error::Error::String(format!("glob error {:?}", e))),
        };
        let filename = file
          .as_path()
          .file_name()
          .and_then(|x| x.to_str())
          .unwrap_or("meh.txt")
          .to_string();
        let (noteid, fid) = make_file_note(&conn, uid, &filename, file.as_path())?;

        // yeetfile table entry.
        conn.execute(
          "insert into yeetfile (yeetkey, audio, filename, fileid)
                   values (?1, ?2, ?3, ?4)",
          params![hv.v, true, filename, fid],
        )?;

        // return zknoteedit.
        let zne = read_zknoteedit(&conn, uid, noteid)?;

        let znew = ZkNoteEditWhat {
          what: "yeet".to_string(),
          zne: zne,
        };

        info!(
          "user#yeet-new-zknote: {} - {}",
          znew.zne.zknote.id.clone(),
          znew.zne.zknote.title.clone()
        );

        return Ok(znew);
      } else {
        Err(orgauth::error::Error::String(format!(
          "yeet err {:?}",
          exit_code
        )))
      }
    }
    Err(e) => Err(orgauth::error::Error::String(format!("yeet err {:?}", e))),
  }
}

// pub fn yeet(
//   conn: &Connection,
//   uid: i64,
//   savedir: &Path,
//   yeet: Yeet,
// ) -> Result<ZkNoteEditWhat, orgauth::error::Error> {
//   // parse 'url'
//   let uri: Uri = match yeet.url.parse() {
//     Ok(uri) => uri,
//     Err(e) => return Err(orgauth::error::Error::String(format!("yeet err {:?}", e))),
//   };

//   // get 'v' parameter.
//   let query = match uri.path_and_query() {
//     Some(paq) => {
//       println!("paq: {:?}", paq);
//       match paq.query() {
//         Some(query) => query,
//         None => {
//           return Err(orgauth::error::Error::String(
//             "query string not present in url".to_string(),
//           ))
//         }
//       }
//     }
//     None => {
//       return Err(orgauth::error::Error::String(format!(
//         "query string not present in url"
//       )))
//     }
//   };

//   // with 'v' paramter, scan for existing files.
//   let hv = match web::Query::<HasV>::from_query(query) {
//     Ok(hv) => hv,
//     Err(e) => {
//       return Err(orgauth::error::Error::String(format!(
//         "query parse error {:?}",
//         e
//       )))
//     }
//   };

//   // if there's already a file, return a new zknote that points at it.
//   match conn.query_row(
//     "select fileid, filename from yeetfile where 
//       yeetkey = ?1 and audio = ?2",
//     params![hv.v, yeet.audio],
//     |row| Ok((row.get(0)?, row.get(1)?)),
//   ) {
//     Ok((fileid, filename)) => {
//       // now make a new note.
//       let sn = save_zknote(
//         &conn,
//         uid,
//         &SaveZkNote {
//           id: None,
//           title: filename,
//           pubid: None,
//           content: "".to_string(),
//           editable: false,
//           showtitle: false,
//           deleted: false,
//         },
//       )?;

//       // set the file id in that note.
//       set_zknote_file(&conn, sn.id, fileid)?;

//       // return zknoteedit.
//       let note = read_zknoteedit(&conn, uid, sn.id)?;

//       let znew = ZkNoteEditWhat {
//         what: "yeet".to_string(),
//         zne: note,
//       };

//       info!(
//         "user#yeet-copy-zknote: {} - {}",
//         znew.zne.zknote.id.clone(),
//         znew.zne.zknote.title.clone()
//       );

//       return Ok(znew);
//     }
//     Err(rusqlite::Error::QueryReturnedNoRows) => (),
//     Err(x) => return Err(x.into()),
//   };

//   let mut child = Command::new("yt-dlp")
//     .arg("-x")
//     .arg(format!("-o{}/%(title)s-%(id)s.%(ext)s", savedir.display()))
//     .arg(yeet.url.clone())
//     .spawn()
//     .expect("youtube-dl failed to execute");

//   match child.wait() {
//     Ok(exit_code) => {
//       if exit_code.success() {
//         // find the yeeted file by 'v'.
//         let file: PathBuf = match glob::glob(format!("{}/*{}*", savedir.display(), hv.v).as_str()) {
//           Ok(mut paths) => match paths.next() {
//             Some(rpb) => match rpb {
//               Ok(pb) => pb,
//               Err(e) => return Err(orgauth::error::Error::String(format!("glob error {:?}", e))),
//             },
//             None => {
//               return Err(orgauth::error::Error::String(format!(
//                 "yeet file not found {:?}",
//                 hv.v
//               )))
//             }
//           },
//           Err(e) => return Err(orgauth::error::Error::String(format!("glob error {:?}", e))),
//         };
//         let filename = file
//           .as_path()
//           .file_name()
//           .and_then(|x| x.to_str())
//           .unwrap_or("meh.txt")
//           .to_string();
//         let (noteid, fid) = make_file_note(&conn, uid, &filename, file.as_path())?;

//         // yeetfile table entry.
//         conn.execute(
//           "insert into yeetfile (yeetkey, audio, filename, fileid)
//                    values (?1, ?2, ?3, ?4)",
//           params![hv.v, true, filename, fid],
//         )?;

//         // return zknoteedit.
//         let zne = read_zknoteedit(&conn, uid, noteid)?;

//         let znew = ZkNoteEditWhat {
//           what: "yeet".to_string(),
//           zne: zne,
//         };

//         info!(
//           "user#yeet-new-zknote: {} - {}",
//           znew.zne.zknote.id.clone(),
//           znew.zne.zknote.title.clone()
//         );

//         return Ok(znew);
//       } else {
//         Err(orgauth::error::Error::String(format!(
//           "yeet err {:?}",
//           exit_code
//         )))
//       }
//     }
//     Err(e) => Err(orgauth::error::Error::String(format!("yeet err {:?}", e))),
//   }
// }
