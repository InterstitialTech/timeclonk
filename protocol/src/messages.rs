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

#[derive(Serialize, ElmEncode, Elm)]
pub enum ServerResponseX {
  ProjectEdit(ProjectEdit),
  ProjectEditDenied,
  SavedProjectEdit(SavedProjectEdit),
  SavedProjectEditDenied,
  SavedProjectInvoice(Project),
  SavedProjectInvoiceDenied,
  ProjectTime(ProjectTime),
  ProjectTimeDenied,
  ProjectList(Vec<ListProject>),
  UserTime(Vec<TimeEntry>),
  AllUsers(Vec<User>),
}

#[derive(Deserialize, Serialize, Debug)]
pub struct UserMessage {
  pub what: String,
  pub data: Option<serde_json::Value>,
}

#[derive(Elm, ElmDecode, Deserialize, Debug)]
pub enum UserMessageX {
  GetProjectList,
  SaveProjectEdit(SavedProjectEdit),
  GetProjectEdit(i64),
  SaveProjectInvoice(SaveProjectInvoice),
  GetProjectTime(i64),
  SaveProjectTime(SaveProjectTime),
  GetUserTime,
  GetAllUsers,
}

#[derive(Deserialize, Serialize, Debug)]
pub struct PublicMessage {
  pub what: String,
  pub data: Option<serde_json::Value>,
}

#[derive(Elm, ElmDecode, Deserialize, Debug)]
pub enum PublicMessageX {
  GetProjectTime(i64),
}
