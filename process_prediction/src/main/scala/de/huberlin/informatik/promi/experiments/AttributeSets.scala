package de.huberlin.informatik.promi.experiments

object AttributeSets {

  /** ignores all numeric attributes */
  val includeNothing = Seq(
    "Label",
    "year",
    "applicant",
    "payment_granted.*",
    "payment_actual.*",
    "cutting.*",
    "rejected",
    "cc")

  /** ignores all monetary values that are derived in the process */
  val includeNoMoney = Seq(
    "Label",
    "year",
    "applicant",
    "payment_granted.*",
    "payment_actual.*",
    "cutting.*")

  /** ignores all monetary values that are calculated (includes cutting)*/
  val includeCutting = Seq(
    "Label",
    "year",
    "applicant",
    "label",
    "payment_granted.*",
    "payment_actual.*",
    "cutting[^0].*")

  /** ignores all monetary values are only available in correction*/
  val includeAll = Seq(
    "Label",
    "year",
    "applicant",
    "payment_granted[^0].*",
    "payment_actual[^0].*",
    "cutting[^0].*")

}