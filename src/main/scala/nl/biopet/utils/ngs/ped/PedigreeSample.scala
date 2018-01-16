package nl.biopet.utils.ngs.ped

case class PedigreeSample(familyId: String,
                          sampleId: String,
                          paternalId: String,
                          maternalId: String,
                          gender: Gender.Value,
                          phenotype: Phenotype.Value) {
  def toPedLine: String =
    List(familyId, sampleId, paternalId, maternalId, gender.id, phenotype.id)
      .mkString("\t")
}

object PedigreeSample {

  private lazy val regex = "[\\t ]".r

  def fromLine(line: String): PedigreeSample = {
    val values = regex.split(line)
    val gender: Gender.Value = values(4) match {
      case "1" => Gender.Male
      case "2" => Gender.Female
      case _ => Gender.Unknown
    }
    val phenotype: Phenotype.Value = values(5) match {
      case "1" => Phenotype.Unaffected
      case "2" => Phenotype.Affected
      case _ => Phenotype.Missing

    }
    PedigreeSample(values(0),
                   values(1),
                   values(2),
                   values(3),
                   gender,
                   phenotype)
  }
}
