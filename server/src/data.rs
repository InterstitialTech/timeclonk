use serde_derive::{Deserialize, Serialize};
use uuid::Uuid;

#[derive(Clone, Deserialize, Serialize, Debug)]
pub struct User {
  pub id: i64,
  pub name: String,
  pub hashwd: String,
  pub salt: String,
  pub email: String,
  pub registration_key: Option<String>,
}

#[derive(Deserialize, Debug)]
pub struct RegistrationData {
  pub uid: String,
  pub pwd: String,
  pub email: String,
}

#[derive(Deserialize, Debug)]
pub struct Login {
  pub uid: String,
  pub pwd: String,
}

#[derive(Deserialize, Debug)]
pub struct ResetPassword {
  pub uid: String,
}

#[derive(Deserialize, Debug)]
pub struct SetPassword {
  pub uid: String,
  pub newpwd: String,
  pub reset_key: Uuid,
}

#[derive(Deserialize, Debug)]
pub struct ChangePassword {
  pub oldpwd: String,
  pub newpwd: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct ChangeEmail {
  pub pwd: String,
  pub email: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct LoginData {
  pub userid: i64,
  pub name: String,
}

// -------------------------------------------------

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ListProject {
  pub id: i64,
  pub name: String,
}

#[derive(Deserialize, Debug, Clone)]
pub struct SaveProject {
  pub id: Option<i64>,
  pub name: String,
  pub description: Option<String>,
  pub public: bool,
}

#[derive(Deserialize, Debug, Clone)]
pub struct SaveProjectMember {
  pub id: i64,
  pub delete: bool,
}

#[derive(Deserialize, Debug, Clone)]
pub struct SaveProjectEdit {
  pub project: SaveProject,
  pub members: Vec<SaveProjectMember>,
}

#[derive(Serialize, Debug, Clone)]
pub struct SavedProject {
  pub id: i64,
  pub changeddate: i64,
}

#[derive(Serialize, Debug, Clone)]
pub struct SavedProjectEdit {
  pub project: SavedProject,
  pub members: Vec<ProjectMember>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Project {
  pub id: i64,
  pub name: String,
  pub description: String,
  pub public: bool,
  pub createdate: i64,
  pub changeddate: i64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProjectMember {
  pub id: i64,
  pub name: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProjectEdit {
  pub project: Project,
  pub members: Vec<ProjectMember>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct TimeEntry {
  pub id: i64,
  pub project: i64,
  pub user: i64,
  pub description: String,
  pub startdate: i64,
  pub enddate: i64,
  pub createdate: i64,
  pub changeddate: i64,
  pub creator: i64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SaveTimeEntry {
  pub id: Option<i64>,
  pub project: i64,
  pub user: i64,
  pub description: String,
  pub startdate: i64,
  pub enddate: i64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SaveProjectTime {
  pub project: i64,
  pub savetimeentries: Vec<SaveTimeEntry>,
  pub deletetimeentries: Vec<i64>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProjectTime {
  pub project: Project,
  pub members: Vec<ProjectMember>,
  pub timeentries: Vec<TimeEntry>,
}
