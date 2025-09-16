package bloop.util

import java.io.ByteArrayInputStream
import java.io.InputStream
import java.nio.file.Path

import bloop.reporter.Problem

import sbt.internal.inc.HashUtil
import sbt.internal.inc.PlainVirtualFileConverter
import xsbti.BasicVirtualFileRef
import xsbti.FileConverter
import xsbti.PathBasedFile
import xsbti.VirtualFile

case class HashedSource(content: Array[Byte], bloopHash: Int, path: Path)
    extends BasicVirtualFileRef(path.toString)
    with VirtualFile
    with PathBasedFile {

  override def contentHash(): Long = HashUtil.farmHash(content)

  override def name(): String = path.getFileName.toString

  override def toPath(): Path = path

  override def input(): InputStream = {
    new ByteArrayInputStream(content)
  }
  override def toString: String = s"VirtualSourceFile($Problem.id@${bloopHash})"
}

object HashedSource {
  val converter: FileConverter = PlainVirtualFileConverter.converter
}
