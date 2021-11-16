use crate::config::Config;
use crate::email;
// use crate::search;
use crate::sqldata;
use crate::util;
use crate::util::is_token_expired;
use actix_session::Session;
use crypto_hash::{hex_digest, Algorithm};
// use either::Either::{Left, Right};
use log::info;
// use simple_error::bail;
use crate::data::{
  ChangeEmail, ChangePassword, Login, LoginData, RegistrationData, ResetPassword, SaveProject,
  SetPassword,
};
use crate::messages::{PublicMessage, ServerResponse, UserMessage};
use std::error::Error;
use std::path::Path;
use uuid::Uuid;

pub fn login_data_for_token(
  session: Session,
  config: &Config,
) -> Result<Option<LoginData>, Box<dyn Error>> {
  let conn = sqldata::connection_open(config.db.as_path())?;

  match session.get("token")? {
    None => Ok(None),
    Some(token) => {
      match sqldata::read_user_by_token(&conn, token, Some(config.login_token_expiration_ms)) {
        Ok(user) => Ok(Some(sqldata::login_data(&conn, user.id)?)),
        Err(_) => Ok(None),
      }
    }
  }
}

pub fn user_interface(
  session: &Session,
  config: &Config,
  msg: UserMessage,
) -> Result<ServerResponse, Box<dyn Error>> {
  info!("got a user message: {}", msg.what);
  let conn = sqldata::connection_open(config.db.as_path())?;
  if msg.what.as_str() == "register" {
    let msgdata = Option::ok_or(msg.data, "malformed registration data")?;
    let rd: RegistrationData = serde_json::from_value(msgdata)?;
    // do the registration thing.
    // user already exists?
    match sqldata::read_user_by_name(&conn, rd.uid.as_str()) {
      Ok(_) => {
        // err - user exists.
        Ok(ServerResponse {
          what: "user exists".to_string(),
          content: serde_json::Value::Null,
        })
      }
      Err(_) => {
        // user does not exist, which is what we want for a new user.
        // get email from 'data'.
        let registration_key = Uuid::new_v4().to_string();
        let salt = util::salt_string();

        // write a user record.
        sqldata::new_user(
          Path::new(&config.db),
          rd.uid.clone(),
          hex_digest(
            Algorithm::SHA256,
            (rd.pwd + salt.as_str()).into_bytes().as_slice(),
          ),
          salt,
          rd.email.clone(),
          registration_key.clone().to_string(),
        )?;

        // send a registration email.
        email::send_registration(
          config.appname.as_str(),
          config.domain.as_str(),
          config.mainsite.as_str(),
          rd.email.as_str(),
          rd.uid.as_str(),
          registration_key.as_str(),
        )?;

        // notify the admin.
        email::send_registration_notification(
          config.appname.as_str(),
          config.domain.as_str(),
          config.admin_email.as_str(),
          rd.email.as_str(),
          rd.uid.as_str(),
          registration_key.as_str(),
        )?;

        Ok(ServerResponse {
          what: "registration sent".to_string(),
          content: serde_json::Value::Null,
        })
      }
    }
  } else if msg.what == "login" {
    let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
    let login: Login = serde_json::from_value(msgdata.clone())?;

    let userdata = sqldata::read_user_by_name(&conn, login.uid.as_str())?;
    match userdata.registration_key {
      Some(_reg_key) => Ok(ServerResponse {
        what: "unregistered user".to_string(),
        content: serde_json::Value::Null,
      }),
      None => {
        if hex_digest(
          Algorithm::SHA256,
          (login.pwd.clone() + userdata.salt.as_str())
            .into_bytes()
            .as_slice(),
        ) != userdata.hashwd
        {
          // don't distinguish between bad user id and bad pwd!
          Ok(ServerResponse {
            what: "invalid user or pwd".to_string(),
            content: serde_json::Value::Null,
          })
        } else {
          let ld = sqldata::login_data(&conn, userdata.id)?;
          // new token here, and token date.
          let token = Uuid::new_v4();
          sqldata::add_token(&conn, userdata.id, token)?;
          session.set("token", token)?;
          sqldata::update_user(&conn, &userdata)?;
          info!("logged in, user: {:?}", userdata.name);

          Ok(ServerResponse {
            what: "logged in".to_string(),
            content: serde_json::to_value(ld)?,
          })
        }
      }
    }
  } else if msg.what == "logout" {
    session.remove("token");

    Ok(ServerResponse {
      what: "logged out".to_string(),
      content: serde_json::Value::Null,
    })
  } else if msg.what == "resetpassword" {
    let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
    let reset_password: ResetPassword = serde_json::from_value(msgdata.clone())?;

    let userdata = sqldata::read_user_by_name(&conn, reset_password.uid.as_str())?;
    match userdata.registration_key {
      Some(_reg_key) => Ok(ServerResponse {
        what: "unregistered user".to_string(),
        content: serde_json::Value::Null,
      }),
      None => {
        let reset_key = Uuid::new_v4();

        // make 'newpassword' record.
        sqldata::add_newpassword(&conn, userdata.id, reset_key.clone())?;

        // send reset email.
        email::send_reset(
          config.appname.as_str(),
          config.domain.as_str(),
          config.mainsite.as_str(),
          userdata.email.as_str(),
          userdata.name.as_str(),
          reset_key.to_string().as_str(),
        )?;

        Ok(ServerResponse {
          what: "resetpasswordack".to_string(),
          content: serde_json::Value::Null,
        })
      }
    }
  } else if msg.what == "setpassword" {
    let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
    let set_password: SetPassword = serde_json::from_value(msgdata.clone())?;

    let mut userdata = sqldata::read_user_by_name(&conn, set_password.uid.as_str())?;
    match userdata.registration_key {
      Some(_reg_key) => Ok(ServerResponse {
        what: "unregistered user".to_string(),
        content: serde_json::Value::Null,
      }),
      None => {
        let npwd = sqldata::read_newpassword(&conn, userdata.id, set_password.reset_key)?;

        if is_token_expired(config.reset_token_expiration_ms, npwd) {
          Ok(ServerResponse {
            what: "password reset failed".to_string(),
            content: serde_json::Value::Null,
          })
        } else {
          userdata.hashwd = hex_digest(
            Algorithm::SHA256,
            (set_password.newpwd + userdata.salt.as_str())
              .into_bytes()
              .as_slice(),
          );
          sqldata::remove_newpassword(&conn, userdata.id, set_password.reset_key)?;
          sqldata::update_user(&conn, &userdata)?;
          Ok(ServerResponse {
            what: "setpasswordack".to_string(),
            content: serde_json::Value::Null,
          })
        }
      }
    }
  } else {
    match session.get::<Uuid>("token")? {
      None => Ok(ServerResponse {
        what: "not logged in".to_string(),
        content: serde_json::Value::Null,
      }),
      Some(token) => {
        match sqldata::read_user_by_token(&conn, token, Some(config.login_token_expiration_ms)) {
          Err(e) => {
            info!("read_user_by_token error: {:?}", e);

            Ok(ServerResponse {
              what: "invalid user or pwd".to_string(),
              content: serde_json::Value::Null,
            })
          }
          Ok(userdata) => {
            // finally!  processing messages as logged in user.
            user_interface_loggedin(&config, userdata.id, &msg)
          }
        }
      }
    }
  }
}

fn user_interface_loggedin(
  config: &Config,
  uid: i64,
  msg: &UserMessage,
) -> Result<ServerResponse, Box<dyn Error>> {
  match msg.what.as_str() {
    "ChangePassword" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let cp: ChangePassword = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.db.as_path())?;
      sqldata::change_password(&conn, uid, cp)?;
      Ok(ServerResponse {
        what: "changed password".to_string(),
        content: serde_json::Value::Null,
      })
    }
    "ChangeEmail" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let cp: ChangeEmail = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.db.as_path())?;
      let (name, token) = sqldata::change_email(&conn, uid, cp.clone())?;
      // send a confirmation email.
      email::send_newemail_confirmation(
        config.appname.as_str(),
        config.domain.as_str(),
        config.mainsite.as_str(),
        cp.email.as_str(),
        name.as_str(),
        token.to_string().as_str(),
      )?;

      Ok(ServerResponse {
        what: "changed email".to_string(),
        content: serde_json::Value::Null,
      })
    }
    "GetProjectList" => {
      let conn = sqldata::connection_open(config.db.as_path())?;
      let projects = sqldata::project_list(&conn, uid)?;

      Ok(ServerResponse {
        what: "projectlist".to_string(),
        content: serde_json::to_value(projects)?,
      })
    }
    "SaveProject" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let sp: SaveProject = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.db.as_path())?;
      let saved = sqldata::save_project(&conn, uid, sp)?;

      Ok(ServerResponse {
        what: "savedproject".to_string(),
        content: serde_json::to_value(saved)?,
      })
    }
    "GetProject" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let pid: i64 = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.db.as_path())?;
      let project = sqldata::read_project(&conn, uid, pid)?;

      Ok(ServerResponse {
        what: "project".to_string(),
        content: serde_json::to_value(project)?,
      })
    }
    wat => Err(Box::new(simple_error::SimpleError::new(format!(
      "invalid 'what' code:'{}'",
      wat
    )))),
  }
}

// public json msgs don't require login.
pub fn public_interface(
  _config: &Config,
  msg: PublicMessage,
) -> Result<ServerResponse, Box<dyn Error>> {
  info!("process_public_json, what={}", msg.what.as_str());
  match msg.what.as_str() {
    // "getzknote" => {
    //   let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
    //   let id: i64 = serde_json::from_value(msgdata.clone())?;
    //   let conn = sqldata::connection_open(config.db.as_path())?;
    //   let note = sqldata::read_zknote(&conn, None, id)?;
    //   info!("public#getzknote: {} - {}", id, note.title);
    //   Ok(ServerResponse {
    //     what: "zknote".to_string(),
    //     content: serde_json::to_value(ZkNoteEdit {
    //       links: sqldata::read_public_zklinks(&conn, note.id)?,
    //       zknote: note,
    //     })?,
    //   })
    // }
    wat => Err(Box::new(simple_error::SimpleError::new(format!(
      "invalid 'what' code:'{}'",
      wat
    )))),
  }
}
