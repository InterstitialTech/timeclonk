use crate::config::Config;
use crate::data::{InvoiceItem, PrintInvoice};
use crate::sqldata;
use actix_files::NamedFile;
use actix_session::Session;
use actix_web::{
  error::{ErrorInternalServerError, ErrorUnauthorized},
  web, HttpRequest,
};
use std::path::PathBuf;
use std::process::Command;
use uuid::Uuid;

pub const INVOICE_DIR: &str = "invoices";

pub async fn invoice(
  session: Session,
  config: web::Data<Config>,
  item: web::Json<PrintInvoice>,
  _req: HttpRequest,
) -> actix_web::Result<NamedFile> {
  // we logged in?  to prevent randos from making invoices
  let token = match session.get::<Uuid>("token")? {
    None => {
      return Err(ErrorUnauthorized(orgauth::error::Error::String(
        "not logged in".to_string(),
      )))
    }
    Some(t) => t,
  };

  let conn = match sqldata::connection_open(config.orgauth_config.db.as_path()) {
    Err(e) => return Err(ErrorInternalServerError(e)),
    Ok(c) => c,
  };
  let _user = match orgauth::dbfun::read_user_by_token_api(
    &conn,
    token,
    config.orgauth_config.login_token_expiration_ms,
    config.orgauth_config.regen_login_tokens,
  ) {
    Err(e) => {
      return Err(ErrorUnauthorized(e));
    }
    Ok(u) => u,
  };

  let path = run_invoice(item.0).map_err(|e| ErrorInternalServerError(e.to_string()))?; // .map_err(|e| actix_web::Error::fmt(, )

  Ok(NamedFile::open(path)?)
}

pub fn invoice_str(item: &InvoiceItem) -> String {
  format!(
    "
    (
      item: \"{}\",
      dur-min: 0,
      hours: {},
      rate: {},
    ),
    ",
    item.description, item.duration, item.rate
  )
}

pub fn run_invoice(print_invoice: PrintInvoice) -> Result<PathBuf, orgauth::error::Error> {
  let items = print_invoice
    .items
    .iter()
    .map(|item| invoice_str(item))
    .collect::<Vec<String>>()
    .concat();

  let eelines = print_invoice.payee.split('\n').count();
  let erlines = print_invoice.payer.split('\n').count();

  let (payee, payer) = match eelines.cmp(&erlines) {
    std::cmp::Ordering::Less => (
      format!(
        "{}{}",
        print_invoice.payee,
        "\n".to_string().repeat(erlines - eelines)
      ),
      print_invoice.payer,
    ),
    std::cmp::Ordering::Equal => (print_invoice.payee, print_invoice.payer),
    std::cmp::Ordering::Greater => (
      print_invoice.payee,
      format!(
        "{}{}",
        print_invoice.payer,
        "\n".to_string().repeat(eelines - erlines)
      ),
    ),
  };

  let typ = format!(
    "
#import \"../invoice.typ\": *


#let biller = \"{}\"
#let recipient = \"{}\"


#let table-data = ( {} )

#show: invoice.with(
  language: \"en\",
  banner-image: none,
  invoice-id: \"{}\",
  // Set this to create a cancellation invoice
  // cancellation-id: \"2024-03-24t210835\",
  issuing-date: \"{}\",
  due-date: {},
  extraFields: {},
  biller: biller,
  hourly-rate: 100,
  recipient: recipient,
  tax: 0,
  items: table-data,
  styling: ( font: none ), // Explicitly use Typst's default font
)",
    payee,
    payer,
    items,
    print_invoice.id,
    print_invoice.date,
    print_invoice
      .due_date
      .map(|dd| format!("\"{}\"", dd))
      .unwrap_or("none".to_string()),
    format!(
      "( {} )",
      print_invoice
        .extra_fields
        .iter()
        .map(|ef| -> String { format!("(\"{}\", \"{}\"), ", ef.n, ef.v) })
        .collect::<Vec<String>>()
        .join("")
    ),
  );

  let invoicepath = format!("{}/{}{}", INVOICE_DIR, print_invoice.id, ".typ");
  let invoicepdf = format!("{}/{}{}", INVOICE_DIR, print_invoice.id, ".pdf");

  orgauth::util::write_string(invoicepath.as_str(), typ.as_str())?;

  let mut child = Command::new("typst");
  child
    .arg("compile")
    .arg(invoicepath.to_string())
    .arg("--root")
    .arg(".");

  let mut res = child.spawn()?;

  match res.wait() {
    Ok(exit_code) => {
      if exit_code.success() {
        // add file to result.
        Ok(invoicepdf.into())
      } else {
        Err(orgauth::error::Error::String(format!(
          "typst err {:?}",
          exit_code
        )))
      }
    }
    Err(e) => Err(orgauth::error::Error::String(format!(
      "invoice err {:?}",
      e
    ))),
  }
}
