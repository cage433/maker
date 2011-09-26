package starling.reports.pivot

import collection.Seq
import starling.pivot._
import starling.utils.ImplicitConversions._

/**
 * Data source that wraps ReportPivotTableDataSource's to add slides.
 */
class SlideReportTableDataSource(reportTableDataSources:List[(List[Int], ReportPivotTableDataSource)], slideLabels:List[String]) extends PivotTableDataSource {
  val slideReport = reportTableDataSources.exists(!_._1.isEmpty)
  val numberOfSlides = reportTableDataSources.maximum(_._1.size)

  val nonShiftedReport = if (slideReport) reportTableDataSources.filter(_._1.forall(_ == 0))(0)._2 else reportTableDataSources.filter(_._1.isEmpty)(0)._2

  val slideFields = (0 until numberOfSlides).map(num => FieldDetails("Slide " + (num + 1) + " (" + slideLabels(num) + ")")).toList
  val normalFields = nonShiftedReport.fieldDetails
  private val normalFieldGroups = nonShiftedReport.fieldDetailsGroups
  
  val fieldDetailsGroups = {
    if (slideReport) {
      FieldDetailsGroup("Slide Fields", slideFields) :: normalFieldGroups
    } else {
      normalFieldGroups
    }
  }

  override def initialState = {
    nonShiftedReport.initialState
  }

  override def drillDownGroups = {
    if (!slideReport) {
      nonShiftedReport.drillDownGroups
    } else {
      // TODO [25 May 2010] put in proper drill down groups.
      nonShiftedReport.drillDownGroups
    }
  }

  def data(pfs : PivotFieldsState) = {
    val lSlideFields = slideFields.map(_.field)
    val slidePossibleValues = lSlideFields.zipWithIndex.map { case(field, index) =>
      field -> reportTableDataSources.map(_._1(index)).toSet.toList
    }.toMap
    val noSlideFields = pfs.allFieldsUsed.intersect(lSlideFields).isEmpty
    if (!slideReport || noSlideFields) {
      nonShiftedReport.data(pfs) //Should sort of sum the shifted reports is there are no slide pivots but what does that mean?
    } else {
      val slideFilters = {
        val filters = pfs.filters.toMap
        slideFields.map { fd => fd -> filters.getOrElse(fd.field, AllSelection) }
      }
      val filteredSlides = reportTableDataSources.filter {
        case (shifts, _) => {
          slideFilters.zipWithIndex.forall { case((fd,selection),index) => selection.matches(fd, shifts(index)) } 
        }
      }
      val allSlideResults = filteredSlides.map(tuple => {
        val (stepDetails, reportTableDataSource) = tuple
        val result = reportTableDataSource.data(pfs.removeAll(lSlideFields.toSet))

        // slide fields, -1, 0, +1 etc.
        val extraFields = Map() ++ stepDetails.zipWithIndex.map(tuple => {
          val (stepNumber, index) = tuple
          slideFields(index).field -> stepNumber
        })

        (result.data.map(_ ++ extraFields), result.possibleValues)
      })
      val rows = allSlideResults.flatMap(_._1)

      // For slides we don't want to show the sum of a measure field if it is not split by the slide param.
      // For example:
      //   -----------------------
      //  | Slide1  |             |
      //  | ------- |  Position   |
      //  | P&L     |             |
      //   -----------------------
      // In the above case we want to see P&L for each slide 1 entry but we want to see Position as the
      // unslid value.
      val filteredRows = rows.map {
        map => {
          map.filter {
            case (field, value) if pfs.columns.measureFields.contains(field) => {
              lSlideFields.forall {
                case f if !(pfs.columns.hasPathContaining(Set(field, f)) || pfs.rowFields.contains(f))=> {
                  val value = map(f)
                  value.asInstanceOf[Int] == 0 // this is the 'unslid' position
                }
                case _ => true
              }
            }
            case _ => true
          }
        }
      }

      val reportFieldsPossibleValues = nonShiftedReport.data(pfs.removeAll(lSlideFields.toSet)).possibleValues
      PivotResult(filteredRows, slidePossibleValues ++ (if (filteredRows.isEmpty) reportFieldsPossibleValues.map{ case(f,_) => f->List()} else reportFieldsPossibleValues ))
    }
  }

  override def availablePages = nonShiftedReport.availablePages

  override def reportSpecificOptions = nonShiftedReport.reportSpecificOptions
}