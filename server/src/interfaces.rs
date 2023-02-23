use crate::config::Config;
use crate::data::{Role, SaveProjectEdit, SaveProjectTime};
use crate::messages::{PublicMessage, ServerResponse, UserMessage};
use crate::sqldata;
use actix_session::Session;
use log::info;
use std::error::Error;

pub fn login_data_for_token(
  session: Session,
  config: &Config,
) -> Result<Option<orgauth::data::LoginData>, Box<dyn Error>> {
  let mut conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
  match session.get("token")? {
    None => Ok(None),
    Some(token) => {
      match orgauth::dbfun::read_user_with_token_pageload(
        &mut conn,
        &session,
        token,
        config.orgauth_config.regen_login_tokens,
        config.orgauth_config.login_token_expiration_ms,
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
    "GetProjectList" => {
      // user can see all their projects.
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
      let allowed = match sp.project.id {
        None => true, // new project
        Some(pid) => match sqldata::member_role(&conn, uid, pid)? {
          Some(Role::Admin) => true,
          _ => false,
        },
      };

      if allowed {
        let saved = sqldata::save_project_edit(&conn, uid, sp)?;
        Ok(ServerResponse {
          what: "savedprojectedit".to_string(),
          content: serde_json::to_value(saved)?,
        })
      } else {
        Ok(ServerResponse {
          what: "saveprojectedit_denied".to_string(),
          content: serde_json::Value::Null,
        })
      }
    }
    "GetProjectEdit" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let pid: i64 = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, pid)? {
        Some(_) => true, // any role is ok
        _ => false,
      };
      if allowed {
        let project = sqldata::read_project_edit(&conn, pid)?;

        Ok(ServerResponse {
          what: "projectedit".to_string(),
          content: serde_json::to_value(project)?,
        })
      } else {
        Ok(ServerResponse {
          what: "projectedit_denied".to_string(),
          content: serde_json::Value::Null,
        })
      }
    }
    "GetProjectTime" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let pid: i64 = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, pid)? {
        Some(_) => true, // any role is ok
        _ => false,
      };

      if allowed {
        let project = sqldata::read_project_time(&conn, pid)?;

        Ok(ServerResponse {
          what: "projecttime".to_string(),
          content: serde_json::to_value(project)?,
        })
      } else {
        Ok(ServerResponse {
          what: "projecttime_denied".to_string(),
          content: serde_json::Value::Null,
        })
      }
    }
    "SaveProjectTime" => {
      // TODO:
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let spt: SaveProjectTime = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;

      let allowed = match sqldata::member_role(&conn, uid, spt.project)? {
        Some(Role::Admin) => true,
        Some(Role::Member) => true,
        _ => false,
      };

      if allowed {
        let bak = sqldata::save_project_time(&conn, uid, spt)?;

        Ok(ServerResponse {
          what: "projecttime".to_string(),
          content: serde_json::to_value(bak)?,
        })
      } else {
        Ok(ServerResponse {
          what: "projecttime_denied".to_string(),
          content: serde_json::Value::Null,
        })
      }
    }
    "GetUserTime" => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let time = sqldata::user_time(&conn, uid)?;
      Ok(ServerResponse {
        what: "usertime".to_string(),
        content: serde_json::to_value(time)?,
      })
    }
    "GetAllUsers" => {
      // all users can see all users!
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let members = sqldata::user_list(&conn)?;

      Ok(ServerResponse {
        what: "allusers".to_string(),
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
  config: &Config,
  msg: PublicMessage,
) -> Result<ServerResponse, Box<dyn Error>> {
  info!("process_public_json, what={}", msg.what.as_str());
  match msg.what.as_str() {
    "GetProjectTime" => {
      let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      let pid: i64 = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let project = sqldata::read_project_time(&conn, pid)?;

      if project.project.public {
        Ok(ServerResponse {
          what: "projecttime".to_string(),
          content: serde_json::to_value(project)?,
        })
      } else {
        Ok(ServerResponse {
          what: "projecttime-denied".to_string(),
          content: serde_json::Value::Null,
        })
      }
    }
    wat => Err(Box::new(simple_error::SimpleError::new(format!(
      "invalid 'what' code:'{}'",
      wat
    )))),
  }
}
