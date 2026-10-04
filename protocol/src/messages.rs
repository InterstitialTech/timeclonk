use elm_rs::{Elm, ElmDecode, ElmEncode};
use serde_derive::{Deserialize, Serialize};

use crate::data::{
  ListProject, Project, ProjectEdit, ProjectId, ProjectTime, SaveProjectEdit, SaveProjectInvoice,
  SaveProjectTime, SavedProjectEdit, TimeEntry, User,
};

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
  TrError(TimeClonkError),
}

#[derive(Serialize, ElmDecode, Elm)]
pub enum TimeClonkError {
  TeNotLoggedIn,
  TeInvalidLogin,
  TeOther(String),
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum TcMessageX {
  TmGetProjectList,
  TmSaveProjectEdit(SaveProjectEdit),
  TmGetProjectEdit(ProjectId),
  TmSaveProjectInvoice(SaveProjectInvoice),
  TmGetProjectTime(ProjectId),
  TmSaveProjectTime(SaveProjectTime),
  TmGetUserTime,
  TmGetAllUsers,
}

#[derive(Elm, ElmEncode, Deserialize, Debug)]
pub enum PublicMessageX {
  PmGetProjectTime(ProjectId),
}

#[derive(Serialize, ElmDecode, Elm)]
pub enum PublicResponseX {
  PrProjectTime(ProjectTime),
  PrError(TimeClonkError),
}
