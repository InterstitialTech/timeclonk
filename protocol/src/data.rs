use elm_rs::{Elm, ElmDecode, ElmEncode};
use orgauth::data::UserId;
use serde_derive::{Deserialize, Serialize};
// use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::str::FromStr;

// -------------------------------------------------

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, PartialEq, Eq, Debug, Clone, Copy)]
pub enum ProjectId {
  Pid(i64),
}

impl ProjectId {
  pub fn to_i64(&self) -> &i64 {
    match self {
      ProjectId::Pid(id) => id,
    }
  }
}

impl Into<i64> for ProjectId {
  fn into(self) -> i64 {
    match self {
      ProjectId::Pid(id) => id,
    }
  }
}

impl From<i64> for ProjectId {
  fn from(a: i64) -> Self {
    ProjectId::Pid(a)
  }
}

impl Display for ProjectId {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ProjectId::Pid(id) => write!(f, "{}", id),
    }
  }
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct UserInviteProject {
  pub id: ProjectId,
  pub role: Role,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct UserInviteData {
  pub projects: Vec<UserInviteProject>,
}

#[derive(Elm, ElmDecode, Serialize, Deserialize, Debug, Clone)]
pub struct ListProject {
  pub id: ProjectId,
  pub name: String,
  pub role: Role,
}

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct ExtraField {
  pub n: String,
  pub v: String,
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub struct SaveProjectInvoice {
  pub id: ProjectId,
  pub invoice_seq: i64,
  pub extra_fields: Vec<ExtraField>,
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub struct SaveProject {
  pub id: Option<ProjectId>,
  pub name: String,
  pub description: String,
  pub due_days: Option<i32>,
  pub extra_fields: Vec<ExtraField>,
  pub invoice_id_template: String,
  pub invoice_seq: i64,
  pub payer: String,
  pub payee: String,
  pub generic_task: String,
  pub public: bool,
  pub rate: Option<f64>,
  pub currency: Option<String>,
}

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub enum Role {
  Member,
  Admin,
  Observer,
}

impl fmt::Display for Role {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{:?}", self)
  }
}

impl FromStr for Role {
  type Err = ();

  fn from_str(input: &str) -> Result<Role, Self::Err> {
    match input {
      "Member" => Ok(Role::Member),
      "Admin" => Ok(Role::Admin),
      "Observer" => Ok(Role::Observer),
      _ => Err(()),
    }
  }
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub struct SaveProjectMember {
  pub id: UserId,
  pub delete: bool,
  pub role: Role,
}

#[derive(Elm, ElmEncode, Deserialize, Debug, Clone)]
pub struct SaveProjectEdit {
  pub project: SaveProject,
  pub members: Vec<SaveProjectMember>,
}

#[derive(Serialize, Debug, Clone)]
pub struct SavedProject {
  pub id: ProjectId,
  pub changeddate: i64,
}

#[derive(Elm, ElmDecode, ElmEncode, Deserialize, Serialize, Debug, Clone)]
pub struct SavedProjectEdit {
  pub project: Project,
  pub members: Vec<ProjectMember>,
}

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct Project {
  pub id: ProjectId,
  pub name: String,
  pub description: String,
  pub due_days: Option<i32>,
  pub extra_fields: Vec<ExtraField>,
  pub invoice_id_template: String,
  pub invoice_seq: i64,
  pub payer: String,
  pub payee: String,
  pub generic_task: String,
  pub public: bool,
  pub rate: Option<f64>,
  pub currency: Option<String>,
  pub createdate: i64,
  pub changeddate: i64,
}

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct ProjectMember {
  pub id: UserId,
  pub name: String,
  pub role: Role,
}

#[derive(Elm, ElmDecode, Serialize, Deserialize, Debug, Clone)]
pub struct User {
  pub id: UserId,
  pub name: String,
}

#[derive(Elm, ElmDecode, Serialize, Deserialize, Debug, Clone)]
pub struct ProjectEdit {
  pub project: Project,
  pub members: Vec<ProjectMember>,
}

#[derive(Elm, ElmEncode, ElmDecode, Serialize, Deserialize, Debug, Clone)]
pub struct TimeEntry {
  pub id: i64,
  pub project: ProjectId,
  pub user: UserId,
  pub description: String,
  pub startdate: i64,
  pub enddate: i64,
  pub ignore: bool,
  pub createdate: i64,
  pub changeddate: i64,
  pub creator: UserId,
}

#[derive(Elm, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct SaveTimeEntry {
  pub id: Option<i64>,
  pub project: ProjectId,
  pub user: UserId,
  pub description: String,
  pub startdate: i64,
  pub enddate: i64,
  pub ignore: bool,
}

#[derive(Elm, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct SaveProjectTime {
  pub project: ProjectId,
  pub savetimeentries: Vec<SaveTimeEntry>,
  pub deletetimeentries: Vec<i64>,
  pub savepayentries: Vec<SavePayEntry>,
  pub deletepayentries: Vec<i64>,
  pub saveallocations: Vec<SaveAllocation>,
  pub deleteallocations: Vec<i64>,
}

#[derive(Elm, ElmEncode, ElmDecode, Serialize, Deserialize, Debug, Clone)]
pub struct ProjectTime {
  pub project: Project,
  pub members: Vec<ProjectMember>,
  pub timeentries: Vec<TimeEntry>,
  pub payentries: Vec<PayEntry>,
  pub allocations: Vec<Allocation>,
}

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub enum PayType {
  Invoiced,
  Paid,
}

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct PayEntry {
  pub id: i64,
  pub project: ProjectId,
  pub user: UserId,
  pub duration: i64,
  pub paytype: PayType,
  pub paymentdate: i64,
  pub description: String,
  pub createdate: i64,
  pub changeddate: i64,
  pub creator: UserId,
}

#[derive(Elm, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct SavePayEntry {
  pub id: Option<i64>,
  pub project: ProjectId,
  pub user: UserId,
  pub duration: i64,
  pub paytype: PayType,
  pub paymentdate: i64,
  pub description: String,
}

#[derive(Elm, ElmDecode, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct Allocation {
  pub id: i64,
  pub project: ProjectId,
  pub duration: i64,
  pub allocationdate: i64,
  pub description: String,
  pub createdate: i64,
  pub changeddate: i64,
  pub creator: UserId,
}

#[derive(Elm, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct SaveAllocation {
  pub id: Option<i64>,
  pub project: ProjectId,
  pub duration: i64,
  pub allocationdate: i64,
  pub description: String,
}

#[derive(Elm, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct PrintInvoice {
  pub id: String,
  pub payer: String,
  pub payee: String,
  pub items: Vec<InvoiceItem>,
  pub date: String,
  pub due_date: Option<String>,
  pub extra_fields: Vec<ExtraField>,
  pub currency: String,
}

#[derive(Elm, ElmEncode, Serialize, Deserialize, Debug, Clone)]
pub struct InvoiceItem {
  pub description: String,
  pub duration: f64,
  pub rate: f64,
}
