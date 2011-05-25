package starling.services

/**
 * Summary information about which curves were committed to the Forward Curve Database.
 */
case class ForwardCurveCommitResponse private (data : Either[String, String])

object ForwardCurveCommitResponse {
  def error(message : String) = ForwardCurveCommitResponse(Left(message))
  def success(message : String) = ForwardCurveCommitResponse(Right(message))
}
