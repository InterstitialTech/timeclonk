use crate::config::Config;
use crate::sqldata;
use actix_session::Session;
use log::info;
use protocol::data::{ProjectId, Role, SaveProjectEdit, SaveProjectInvoice, SaveProjectTime};
use protocol::messages::{PublicMessageX, PublicResponseX, TcMessageX, TcResponseX};
use std::error::Error;

pub fn login_data_for_token(
  session: Session,
  config: &Config,
) -> Result<Option<orgauth::data::LoginData>, orgauth::error::Error> {
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
  uid: orgauth::data::UserId,
  msg: &TcMessageX,
) -> Result<TcResponseX, Box<dyn Error>> {
  match msg {
    TcMessageX::TmGetProjectList => {
      // user can see all their projects.
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let projects = sqldata::project_list(&conn, uid)?;

      Ok(TcResponseX::TrProjectList(projects))
    }
    TcMessageX::TmSaveProjectEdit(sp) => {
      // let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      // let sp: SaveProjectEdit = serde_json::from_value(msgdata.clone())?;

      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sp.project.id {
        None => true, // new project
        Some(pid) => match sqldata::member_role(&conn, uid, &pid)? {
          Some(Role::Admin) => true,
          _ => false,
        },
      };

      if allowed {
        let saved = sqldata::save_project_edit(&conn, uid, sp)?;
        Ok(TcResponseX::TrSavedProjectEdit(saved))
      } else {
        Ok(TcResponseX::TrSavedProjectEditDenied)
      }
    }
    TcMessageX::TmGetProjectEdit(pid) => {
      // let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      // let pid: ProjectId = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, pid)? {
        Some(_) => true, // any role is ok
        _ => false,
      };
      if allowed {
        let project = sqldata::read_project_edit(&conn, pid)?;

        Ok(TcResponseX::TrProjectEdit(project))
      } else {
        Ok(TcResponseX::TrProjectEditDenied)
      }
    }
    TcMessageX::TmSaveProjectInvoice(sp) => {
      // let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      // let sp: SaveProjectInvoice = serde_json::from_value(msgdata.clone())?;

      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, &sp.id)? {
        Some(Role::Admin) => true,
        Some(Role::Member) => true,
        _ => false,
      };

      if allowed {
        let saved = sqldata::save_project_invoice(&conn, sp)?;
        Ok(TcResponseX::TrSavedProjectInvoice(saved))
      } else {
        Ok(TcResponseX::TrSavedProjectInvoiceDenied)
      }
    }
    TcMessageX::TmGetProjectTime(pid) => {
      // let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      // let pid: ProjectId = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, pid)? {
        Some(_) => true, // any role is ok
        _ => false,
      };

      if allowed {
        let project = sqldata::read_project_time(&conn, pid)?;

        Ok(TcResponseX::TrProjectTime(project))
      } else {
        Ok(TcResponseX::TrProjectTimeDenied)
      }
    }
    TcMessageX::TmSaveProjectTime(spt) => {
      // TODO:
      // let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      // let spt: SaveProjectTime = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;

      let allowed = match sqldata::member_role(&conn, uid, &spt.project)? {
        Some(Role::Admin) => true,
        Some(Role::Member) => true,
        _ => false,
      };

      if allowed {
        let bak = sqldata::save_project_time(&conn, uid, spt)?;

        Ok(TcResponseX::TrProjectTime(bak))
      } else {
        Ok(TcResponseX::TrProjectTimeDenied)
      }
    }
    TcMessageX::TmGetUserTime => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let time = sqldata::user_time(&conn, uid)?;
      Ok(TcResponseX::TrUserTime(time))
    }
    TcMessageX::TmGetAllUsers => {
      // all users can see all users!
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let members = sqldata::user_list(&conn)?;

      Ok(TcResponseX::TrAllUsers(members))
    }
  }
}

// public json msgs don't require login.
pub fn public_interface(
  config: &Config,
  msg: PublicMessageX,
) -> Result<PublicResponseX, Box<dyn Error>> {
  match msg {
    PublicMessageX::PmGetProjectTime(pid) => {
      // let msgdata = Option::ok_or(msg.data.as_ref(), "malformed json data")?;
      // let pid: ProjectId = serde_json::from_value(msgdata.clone())?;
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let project = sqldata::read_project_time(&conn, &pid)?;

      if project.project.public {
        Ok(PublicResponseX::PrProjectTime(project))
      } else {
        Err(Box::new(simple_error::SimpleError::new(format!(
          "can't access project!"
        ))))
      }
    }
  }
}
