#let nbh = "‑"

// Truncate a number to 2 decimal places
// and add trailing zeros if necessary
// E.g. 1.234 -> 1.23, 1.2 -> 1.20
#let add-zeros = (num) => {
    // Can't use trunc and fract due to rounding errors
    let frags = str(num).split(".")
    let (intp, decp) = if frags.len() == 2 { frags } else { (num, "00") }
    str(intp) + "." + (str(decp) + "00").slice(0, 2)
  }

// From https://stackoverflow.com/a/57080936/1850340
#let verify-iban = (country, iban) => {
    let iban-regexes = (
        DE: regex(
          "^DE[a-zA-Z0-9]{2}\s?([0-9]{4}\s?){4}([0-9]{2})$"
        ),
        GB: regex(
          "^GB[a-zA-Z0-9]{2}\s?([a-zA-Z]{4}\s?){1}([0-9]{4}\s?){3}([0-9]{2})$"
        ),
      )

    if country == none or not country in iban-regexes {
      true
    }
    else {
      iban.find(iban-regexes.at(country)) != none
    }
}

#let parse-date = (date-str) => {
  let parts = date-str.split("-")
  if parts.len() != 3 {
    panic(
      "Invalid date string: " + date-str + "\n" +
      "Expected format: YYYY-MM-DD"
    )
  }
  datetime(
    year: int(parts.at(0)),
    month: int(parts.at(1)),
    day: int(parts.at(2)),
  )
}

#let TODO = box(
  inset: (x: 0.5em),
  outset: (y: 0.2em),
  radius: 0.2em,
  fill: rgb(255,180,170),
)[
  #text(
    size: 0.8em,
    weight: 600,
    fill: rgb(100,68,64)
  )[TODO]
]

#let horizontalrule = [
  #v(8mm)
  #line(
    start: (20%,0%),
    end: (80%,0%),
    stroke: 0.8pt + gray,
  )
  #v(8mm)
]

#let signature-line = line(length: 5cm, stroke: 0.4pt)

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#let languages = (
    en: (
      id: "en",
      country: "GB",
      recipient: "Recipient",
      biller: "Biller",
      invoice: "Invoice",
      cancellation-invoice: "Cancellation Invoice",
      cancellation-notice: (id, issuing-date) => [
        As agreed, you will receive a credit note
        for the invoice *#id* dated *#issuing-date*.
      ],
      invoice-id: "Invoice ID",
      issuing-date: "Date",
      items: "Items",
      closing: "Thank you for the good cooperation!",
      number: "№",
      date: "Date",
      due-date: "Due Date",
      item: "Item",
      hours: "Hours",
      rate: "Rate",
      total-time: "Total working time",
      subtotal: "Subtotal",
      discount-of: "Discount of",
      tax: "tax of",
      reverse-charge: "Reverse Charge",
      total: "Total",
      due-text: val =>
        [Please transfer the money onto following bank account due to *#val*:],
      owner: "Owner",
      iban: "IBAN",
    ),
    de: (
      id: "de",
      country: "DE",
      recipient: "Empfänger",
      biller: "Aussteller",
      invoice: "Rechnung",
      cancellation-invoice: "Stornorechnung",
      cancellation-notice: (id, issuing-date) => [
        Vereinbarungsgemäß erhalten Sie hiermit eine Gutschrift
        zur Rechnung *#id* vom *#issuing-date*.
      ],
      invoice-id: "Rechnungsnummer",
      issuing-date: "Ausstellungsdatum",
      items: "Leistungen",
      closing: "Vielen Dank für die gute Zusammenarbeit!",
      number: "Nr",
      date: "Datum",
      due-date: "Due-Date",
      item: "Beschreibung",
      hours: "Menge",
      rate: "Preis",
      total-time: "Gesamtarbeitszeit",
      subtotal: "Zwischensumme",
      discount-of: "Rabatt von",
      tax: "Umsatzsteuer von",
      reverse-charge: "Steuerschuldnerschaft des\nLeistungsempfängers",
      total: "Gesamt",
      due-text: val =>
        [Bitte überweise den Betrag bis *#val* auf folgendes Konto:],
      owner: "Inhaber",
      iban: "IBAN",
    ),
  )

#let invoice(
  language: "en",
  country: none,
  title: none,
  banner-image: none,
  invoice-id: none,
  cancellation-id: none,
  issuing-date: none,
  due-date: none,
  biller: (:),
  recipient: (:),
  keywords: (),
  hourly-rate: none,
  currency-symbol: "$",
  styling: (:), // font, font-size, margin (sets defaults below)
  items: (),
  extraFields: (),
  discount: none,
  tax: 0.19,
  data: none,
  doc,
) = {
  // Set styling defaults
  styling.font = styling.at("font", default: "Liberation Sans")
  styling.font-size = styling.at("font-size", default: 11pt)
  styling.margin = styling.at("margin", default: (
    top: 20mm,
    right: 25mm,
    bottom: 20mm,
    left: 25mm,
  ))

  language = if data != none {
    data.at("language", default: language)
  } else { language }

  // Translations
  let t = if type(language) == str { languages.at(language) }
          else if type(language) == dictionary { language }
          else { panic("Language must be either a string or a dictionary.") }

  if data != none {
    language = data.at("language", default: language)
    country = data.at("country", default: t.country)
    title = data.at("title", default: title)
    banner-image = data.at("banner-image", default: banner-image)
    invoice-id = data.at("invoice-id", default: invoice-id)
    cancellation-id = data.at("cancellation-id", default: cancellation-id)
    issuing-date = data.at("issuing-date", default: issuing-date)
    due-date = data.at("due-date", default: due-date)
    biller = data.at("biller", default: biller)
    recipient = data.at("recipient", default: recipient)
    keywords = data.at("keywords", default: keywords)
    hourly-rate = data.at("hourly-rate", default: hourly-rate)
    currency-symbol = data.at("currency-symbol", default: currency-symbol)
    styling = data.at("styling", default: styling)
    items = data.at("items", default: items)
    extraFields = data.at("extraFields", default: extraFields)
    discount = data.at("discount", default: discount)
    tax = data.at("tax", default: tax)
  }

  // Verify inputs
  // assert(
  //   verify-iban(country, biller.iban),
  //   message: "Invalid IBAN " + biller.iban + " for country " + country
  // )

  let signature = ""
  let issuing-date = if issuing-date != none { issuing-date }
        else { datetime.today().display("[year]-[month]-[day]") }

  set document(
    title: title,
    keywords: keywords,
    date: parse-date(issuing-date),
  )
  set page(
    margin: styling.margin,
    numbering: none,
  )
  set par(justify: true)
  set text(
    lang: t.id,
    font: if styling.font != none { styling.font } else { () },
    size: styling.font-size,
  )
  set table(stroke: none)

  // Offset page top margin for banner image
  [#pad(top: -20mm, banner-image)]

  align(center)[#block(inset: 2em)[
    #text(weight: "bold", size: 2em)[
      #(if title != none { title } else {
        if cancellation-id != none { t.cancellation-invoice }
        else { t.invoice }
      })
    ]
  ]]

  let invoice-id-norm = if invoice-id != none {
          if cancellation-id != none { cancellation-id }
          else { invoice-id }
        }
        else {
          TODO
          // TODO: Reactitaxe after Typst supports hour, minute, and second
          // datetime
          //   .today()
          //   .display("[year]-[month]-[day]t[hour][minute][second]")
        }

  let ddt = if due-date != none {
    (      [#t.due-date:], [*#due-date*])
    } else { () }

  align(center,
    table(
      columns: 2,
      align: (right, left),
      inset: 4pt,
      [#t.invoice-id:], [*#invoice-id-norm*],
      [#t.issuing-date:], [*#issuing-date*],
      ..ddt,
    )
  )

  v(2em)

  box(height: 10em)[
    #columns(2, gutter: 4em)[
      === #t.recipient
      #v(0.5em)
      #recipient
      // .name \
      // #{if "title" in recipient { [#recipient.title \ ] }}
      // #recipient.address.city #recipient.address.postal-code \
      // #recipient.address.street \
      // #{if recipient.tax-id.starts-with("DE"){"USt-IdNr.:"}}
      //   #recipient.tax-id


      === #t.biller
      #v(0.5em)
      #biller
      // .name \
      // #{if "title" in biller { [#biller.title \ ] }}
      // #biller.address.city #biller.address.postal-code \
      // #biller.address.street \
      // #{if biller.tax-id.starts-with("DE"){"USt-IdNr.:"}}
      //   #biller.tax-id
    ]
  ]

  if cancellation-id != none {
    (t.cancellation-notice)(invoice-id, issuing-date)
  }

  v(1em)

  let getRowTotal = row => {
    row.rate * row.at("hours", default: 1)
  }

  let cancel-neg = if cancellation-id != none { -1 } else { 1 }

  table(
    columns: (auto, auto, auto, auto, auto, auto, auto),
    align: (col, row) => right,
    inset: 6pt,
    table.header(
      // TODO: Add after https://github.com/typst/typst/issues/3734
      // align: (right,left,left,center,center,center,center,),
      table.hline(stroke: 0.5pt),
      [*#t.item*],
      [*#t.hours*],
      [*#t.rate*\ #text(size: 0.8em)[( #(currency-symbol) )]],
      [*#t.total*\ #text(size: 0.8em)[( #(currency-symbol) )]],
      table.hline(stroke: 0.5pt),
    ),
    ..items
      .enumerate()
      .map(((index, row)) => {
        let dur-min = row.at("dur-min", default: 0)
        let dur-hour = dur-min / 60
        (
          row.item,
          str(row.at("hours", default: if dur-min == 0 { "1" } else { "" })),
          str(add-zeros(cancel-neg *
           row.at("rate", default: calc.round(hourly-rate * dur-hour, digits: 2))
          )),
          str(add-zeros(cancel-neg * getRowTotal(row))),
        )
      })
      .flatten()
      .map(str),
    table.hline(stroke: 0.5pt),
  )

  let sub-total = items
        .map(getRowTotal)
        .sum()

  let total-duration = items
        .map(row => int(row.at("dur-min", default: 0)))
        .sum()

  let discount-value = if discount == none { 0 }
    else {
      if (discount.type == "fixed") { discount.value }
      else if discount.type == "proportionate" {
        sub-total * discount.value
      }
      else { panic(["#discount.type" is no valid discount type]) }
    }
  let discount-label = if discount == none { 0 }
    else {
      if (discount.type == "fixed") { str(discount.value) + " " + currency-symbol }
      else if discount.type == "proportionate" {
        str(discount.value * 100) + " %"
      }
      else { panic(["#discount.type" is no valid discount type]) }
    }
  let has-reverse-charge = false
  // {
  //       biller.tax-id.slice(0, 2) != recipient.tax-id.slice(0, 2)
  //     }
  let taxamt = if has-reverse-charge { 0 } else { sub-total * tax }
  let total = sub-total - discount-value + taxamt

  let table-entries = (
    if total-duration != 0 {
      ([#t.total-time:], [*#total-duration min*])
    },
    if (discount-value != 0) or (tax != 0) {
      ([#t.subtotal:],
      [#{add-zeros(cancel-neg * sub-total)} #(currency-symbol)])
    },
    if discount-value != 0 {
      (
        [#t.discount-of #discount-label
          #{if discount.reason != "" { "(" + discount.reason + ")" }}],
        [-#add-zeros(cancel-neg * discount-value) #(currency-symbol)]
      )
    },
    if not has-reverse-charge and (tax != 0) {
      ([#t.tax #{tax * 100} %:],
        [#{add-zeros(cancel-neg * taxamt)} #(currency-symbol)]
      )
    },
    if (has-reverse-charge) {
      ([#t.tax:], text(0.9em)[#t.reverse-charge])
    },
    (
      [*#t.total*:],
      [*#add-zeros(cancel-neg * total) #(currency-symbol)*]
    ),
  )
  .filter(entry => entry != none)

  let grayish = luma(245)

  align(right,
    table(
      columns: 2,
      fill: (col, row) => // if last row
        if row == table-entries.len() - 1 { grayish }
        else { none },
      stroke: (col, row) => // if last row
        if row == table-entries.len() - 1 { (y: 0.5pt, x: 0pt) }
        else { none },
      ..table-entries
        .flatten(),
    )
  )

  v(1em)

  if cancellation-id == none {
    let due-date = if due-date != none { due-date }
          else {
            (parse-date(issuing-date) + duration(days: 14))
              .display("[year]-[month]-[day]")
          }

    // (t.due-text)(due-date)
    align(left,
      table(
        columns: 2,
        align: (left, left),
        inset: 4pt,
        ..extraFields
          .map((row) => {
            (row.at(0),  row.at(1))
          }
          ).flatten()
      )
    )


    v(1em)
    align(center)[
      #table(
        fill: grayish,
        // stroke: 1pt + blue,
        // columns: 2, // TODO: Doesn't work for unknown reason
        columns: (8em, auto),
        inset: (col, row) =>
          if col == 0 {
            if row == 0 { (top: 1.2em, right: 0.6em, bottom: 0.6em) }
            else { (top: 0.6em, right: 0.6em, bottom: 1.2em) }
          }
          else {
            if row == 0 { (top: 1.2em, right: 2em, bottom: 0.6em, left: 0.6em) }
            else { (top: 0.6em, right: 2em, bottom: 1.2em, left: 0.6em) }
          },
        align: (col, row) => (right,left,).at(col),
        table.hline(stroke: 0.5pt),
        // [#t.owner:], [*#biller.name*],
        // [#t.iban:], [*#biller.iban*],
        table.hline(stroke: 0.5pt),
      )
    ]
    v(1em)

    // t.closing
  }
  else {
    v(1em)
    align(center, strong(t.closing))
  }

  doc
}
