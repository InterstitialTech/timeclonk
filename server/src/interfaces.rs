use crate::config::Config;
use crate::data::{SaveProjectEdit, SaveProjectTime};
use crate::email;
use crate::messages::{PublicMessage, ServerResponse, UserMessage};
use crate::sqldata;
use crate::util;
use crate::util::is_token_expired;
use actix_session::Session;
use crypto_hash::{hex_digest, Algorithm};
use log::info;
use orgauth::data::LoginData;
use orgauth::data::{ChangeEmail, ChangePassword};
use std::error::Error;
use std::path::Path;
use uuid::Uuid;

pub fn login_data_for_token(
  session: Session,
  config: &Config,
) -> Result<Option<orgauth::data::LoginData>, Box<dyn Error>> {
  let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
  match session.get("token")? {
    None => Ok(None),
    Some(token) => {
      match orgauth::dbfun::read_user_by_token(
        &conn,
        token,
        Some(config.orgauth_config.login_token_expiration_ms),
      ) {
        Ok(user) => Ok(Some(orgauth::dbfun::login_data(&conn, user.id)?)),
        Err(e) => Err(e),
      }
    }
  }
}

pub fn timeclonk_interface_loggedin(
  config: &Config,
  uid: i64,
  msg: &UserMessage,
) -> Result<ServerResponse, Box<dyn Error>> {
  match msg.what.as_str() {
    "ChangePassword" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let cp: ChangePassword = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      sqldata::change_password(&conn, uid, cp)?;
      Ok(ServerResponse {
        what: "changed password".to_string(),
        content: serde_json::Value::Null,
      })
    }
    "ChangeEmail" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let cp: ChangeEmail = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let (name, token) = sqldata::change_email(&conn, uid, cp.clone())?;
      // send a confirmation email.
      email::send_newemail_confirmation(
        config.orgauth_config.appname.as_str(),
        config.orgauth_config.domain.as_str(),
        config.orgauth_config.mainsite.as_str(),
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
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let projects = sqldata::project_list(&conn, uid)?;

      Ok(ServerResponse {
        what: "projectlist".to_string(),
        content: serde_json::to_value(projects)?,
      })
    }
    "SaveProjectEdit" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let sp: SaveProjectEdit = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let saved = sqldata::save_project_edit(&conn, uid, sp)?;

      Ok(ServerResponse {
        what: "savedprojectedit".to_string(),
        content: serde_json::to_value(saved)?,
      })
    }
    "GetProjectEdit" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let pid: i64 = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let project = sqldata::read_project_edit(&conn, uid, pid)?;

      Ok(ServerResponse {
        what: "projectedit".to_string(),
        content: serde_json::to_value(project)?,
      })
    }
    "GetProjectTime" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let pid: i64 = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let project = sqldata::read_project_time(&conn, uid, pid)?;

      Ok(ServerResponse {
        what: "projecttime".to_string(),
        content: serde_json::to_value(project)?,
      })
    }
    "SaveProjectTime" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let spt: SaveProjectTime = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let bak = sqldata::save_project_time(&conn, uid, spt)?;

      Ok(ServerResponse {
        what: "projecttime".to_string(),
        content: serde_json::to_value(bak)?,
      })
    }
    "GetAllMembers" => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let members = sqldata::member_list(&conn, uid, None)?;

      Ok(ServerResponse {
        what: "allmembers".to_string(),
        content: serde_json::to_value(members)?,
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
    //   let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
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
