use crate::config::Config;
use crate::sqldata;
use actix_session::Session;
// use log::info;
use protocol::data::Role;
use protocol::messages::{PublicMessage, PublicResponse, TcMessage, TcResponse};
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
  msg: &TcMessage,
) -> Result<TcResponse, Box<dyn Error>> {
  match msg {
    TcMessage::TmGetProjectList => {
      // user can see all their projects.
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let projects = sqldata::project_list(&conn, uid)?;

      Ok(TcResponse::TrProjectList(projects))
    }
    TcMessage::TmSaveProjectEdit(sp) => {
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
        Ok(TcResponse::TrSavedProjectEdit(saved))
      } else {
        Ok(TcResponse::TrSavedProjectEditDenied)
      }
    }
    TcMessage::TmGetProjectEdit(pid) => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, pid)? {
        Some(_) => true, // any role is ok
        _ => false,
      };
      if allowed {
        let project = sqldata::read_project_edit(&conn, pid)?;

        Ok(TcResponse::TrProjectEdit(project))
      } else {
        Ok(TcResponse::TrProjectEditDenied)
      }
    }
    TcMessage::TmSaveProjectInvoice(sp) => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, &sp.id)? {
        Some(Role::Admin) => true,
        Some(Role::Member) => true,
        _ => false,
      };

      if allowed {
        let saved = sqldata::save_project_invoice(&conn, sp)?;
        Ok(TcResponse::TrSavedProjectInvoice(saved))
      } else {
        Ok(TcResponse::TrSavedProjectInvoiceDenied)
      }
    }
    TcMessage::TmGetProjectTime(pid) => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let allowed = match sqldata::member_role(&conn, uid, pid)? {
        Some(_) => true, // any role is ok
        _ => false,
      };

      if allowed {
        let project = sqldata::read_project_time(&conn, pid)?;

        Ok(TcResponse::TrProjectTime(project))
      } else {
        Ok(TcResponse::TrProjectTimeDenied)
      }
    }
    TcMessage::TmSaveProjectTime(spt) => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;

      let allowed = match sqldata::member_role(&conn, uid, &spt.project)? {
        Some(Role::Admin) => true,
        Some(Role::Member) => true,
        _ => false,
      };

      if allowed {
        let bak = sqldata::save_project_time(&conn, uid, spt)?;

        Ok(TcResponse::TrProjectTime(bak))
      } else {
        Ok(TcResponse::TrProjectTimeDenied)
      }
    }
    TcMessage::TmGetUserTime => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let time = sqldata::user_time(&conn, uid)?;
      Ok(TcResponse::TrUserTime(time))
    }
    TcMessage::TmGetAllUsers => {
      // all users can see all users!
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let members = sqldata::user_list(&conn)?;

      Ok(TcResponse::TrAllUsers(members))
    }
  }
}

// public json msgs don't require login.
pub fn public_interface(
  config: &Config,
  msg: PublicMessage,
) -> Result<PublicResponse, Box<dyn Error>> {
  match msg {
    PublicMessage::PmGetProjectTime(pid) => {
      let conn = sqldata::connection_open(config.orgauth_config.db.as_path())?;
      let project = sqldata::read_project_time(&conn, &pid)?;

      if project.project.public {
        Ok(PublicResponse::PrProjectTime(project))
      } else {
        Err(Box::new(simple_error::SimpleError::new(format!(
          "can't access project!"
        ))))
      }
    }
  }
}
