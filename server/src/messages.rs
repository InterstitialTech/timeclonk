use elm_rs::{Elm, ElmDecode, ElmEncode};
use orgauth::data::User;
use serde_derive::{Deserialize, Serialize};
use serde_json::Value;

use crate::data::{
  ListProject, Project, ProjectEdit, ProjectTime, SaveProjectInvoice, SaveProjectTime,
  SavedProjectEdit, TimeEntry,
};

#[derive(Serialize, Deserialize)]
pub struct ServerResponse {
  pub what: String,
  pub content: Value,
}

#[derive(Serialize, ElmDecode, Elm)]
pub enum ServerResponseX {
  SrProjectEdit(ProjectEdit),
  SrProjectEditDenied,
  SrSavedProjectEdit(SavedProjectEdit),
  SrSavedProjectEditDenied,
  SrSavedProjectInvoice(Project),
  SrSavedProjectInvoiceDenied,
  SrProjectTime(ProjectTime),
  SrProjectTimeDenied,
  SrProjectList(Vec<ListProject>),
  SrUserTime(Vec<TimeEntry>),
  SrAllUsers(Vec<User>),
}

#[derive(Deserialize, Serialize, Debug)]
pub struct UserMessage {
  pub what: String,
  pub data: Option<serde_json::Value>,
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum UserMessageX {
  UmGetProjectList,
  UmSaveProjectEdit(SavedProjectEdit),
  UmGetProjectEdit(i64),
  UmSaveProjectInvoice(SaveProjectInvoice),
  UmGetProjectTime(i64),
  UmSaveProjectTime(SaveProjectTime),
  UmGetUserTime,
  UmGetAllUsers,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct PublicMessage {
  pub what: String,
  pub data: Option<serde_json::Value>,
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum PublicMessageX {
  PmGetProjectTime(i64),
}
