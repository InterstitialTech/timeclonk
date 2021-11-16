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

#[derive(Serialize, Debug, Clone)]
pub struct SavedProject {
  pub id: i64,
  pub changeddate: i64,
}

#[derive(Serialize,Deserialize, Debug, Clone)]
pub struct Project {
  pub id: i64,
  pub name: String,
  pub description: String,
  pub public: bool,
  pub createdate: i64,
  pub changeddate: i64,
}

/*
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProjectList {
  pub projects: Vec<ListProject>
}
*/
