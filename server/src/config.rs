use orgauth::data as orgauth_data;
use serde_derive::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct Config {
  pub ip: String,
  pub port: u16,
  pub static_path: Option<PathBuf>,
  pub invoice_template: Option<PathBuf>,
  pub orgauth_config: orgauth_data::Config,
}
