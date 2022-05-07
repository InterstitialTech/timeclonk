use serde_derive::{Deserialize, Serialize};

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
  pub rate: Option<i64>,
  pub currency: Option<String>,
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
  pub project: Project,
  pub members: Vec<ProjectMember>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Project {
  pub id: i64,
  pub name: String,
  pub description: String,
  pub public: bool,
  pub rate: Option<f64>,
  pub currency: Option<String>,
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
  pub ignore: bool,
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
  pub ignore: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SaveProjectTime {
  pub project: i64,
  pub savetimeentries: Vec<SaveTimeEntry>,
  pub deletetimeentries: Vec<i64>,
  pub savepayentries: Vec<SavePayEntry>,
  pub deletepayentries: Vec<i64>,
  pub saveallocations: Vec<SaveAllocation>,
  pub deleteallocations: Vec<i64>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct ProjectTime {
  pub project: Project,
  pub members: Vec<ProjectMember>,
  pub timeentries: Vec<TimeEntry>,
  pub payentries: Vec<PayEntry>,
  pub allocations: Vec<Allocation>,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct PayEntry {
  pub id: i64,
  pub project: i64,
  pub user: i64,
  pub duration: i64,
  pub paymentdate: i64,
  pub description: String,
  pub createdate: i64,
  pub changeddate: i64,
  pub creator: i64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SavePayEntry {
  pub id: Option<i64>,
  pub project: i64,
  pub user: i64,
  pub duration: i64,
  pub paymentdate: i64,
  pub description: String,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Allocation {
  pub id: i64,
  pub project: i64,
  pub duration: i64,
  pub allocationdate: i64,
  pub description: String,
  pub createdate: i64,
  pub changeddate: i64,
  pub creator: i64,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct SaveAllocation {
  pub id: Option<i64>,
  pub project: i64,
  pub duration: i64,
  pub allocationdate: i64,
  pub description: String,
}
