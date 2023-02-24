package sttp.client4

import java.io.File
import java.nio.file.Path

import sttp.client4.internal._

trait PartialRequestExtensions[+R <: PartialRequestBuilder[R, _]] { self: R =>

  /** If content type is not yet specified, will be set to `application/octet-stream`.
    *
    * If content length is not yet specified, will be set to the length of the given file.
    */
  def body(file: File): R = body(SttpFile.fromFile(file))

  /** If content type is not yet specified, will be set to `application/octet-stream`.
    *
    * If content length is not yet specified, will be set to the length of the given file.
    */
  def body(path: Path): R = body(SttpFile.fromPath(path))

  // this method needs to be in the extensions, so that it has lowest priority when considering overloading options
  /** If content type is not yet specified, will be set to `application/octet-stream`.
    */
  def body[B: BodySerializer](b: B): R =
    withBody(implicitly[BodySerializer[B]].apply(b))
}
