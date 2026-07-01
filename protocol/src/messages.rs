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
pub enum TcResponseX {
  TrProjectEdit(ProjectEdit),
  TrProjectEditDenied,
  TrSavedProjectEdit(SavedProjectEdit),
  TrSavedProjectEditDenied,
  TrSavedProjectInvoice(Project),
  TrSavedProjectInvoiceDenied,
  TrProjectTime(ProjectTime),
  TrProjectTimeDenied,
  TrProjectList(Vec<ListProject>),
  TrUserTime(Vec<TimeEntry>),
  TrAllUsers(Vec<User>),
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum TcMessageX {
  TmGetProjectList,
  TmSaveProjectEdit(SavedProjectEdit),
  TmGetProjectEdit(i64),
  TmSaveProjectInvoice(SaveProjectInvoice),
  TmGetProjectTime(i64),
  TmSaveProjectTime(SaveProjectTime),
  TmGetUserTime,
  TmGetAllUsers,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct UserMessage {
  pub what: String,
  pub data: Option<serde_json::Value>,
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

#[derive(Serialize, ElmDecode, Elm)]
pub enum PublicResponseX {
  PrProjectTime(ProjectTime),
}
