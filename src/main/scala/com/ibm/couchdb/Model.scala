/*
 * Copyright 2015 IBM Corporation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ibm.couchdb

import sun.misc.BASE64Decoder

class Config private (val host: String, val port: Int, val https: Boolean, val credentials: Option[(String, String)])
object Config {
  def apply(host: String, port: Int, https: Boolean, credentials: Option[(String, String)]) = new Config(host, port, https, credentials.filter(_._1 != ""))
}

case class CouchDoc[T](doc: T,
                       kind: String,
                       _id: String = "",
                       _rev: String = "",
                       _deleted: Boolean = false,
                       _attachments: Map[String, CouchAttachment] = Map.empty[String, CouchAttachment],
                       _conflicts: Seq[String] = Seq.empty[String],
                       _deleted_conflicts: Seq[String] = Seq.empty[String],
                       _local_seq: Int = 0)

case class CouchDocRev(rev: String)

case class CouchKeyVal[K, V](id: String, key: K, value: V)

case class CouchKeyValWithDoc[K, V, D](id: String, key: K, value: V, doc: CouchDoc[D])

case class CouchKeyVals[K, V](offset: Int, total_rows: Int, rows: Seq[CouchKeyVal[K, V]])

case class CouchDocs[K, V, D](offset: Int, total_rows: Int, rows: Seq[CouchKeyValWithDoc[K, V, D]]) {
  def getDocs: Seq[CouchDoc[D]] = rows.map(_.doc)

  def getDocsData: Seq[D] = rows.map(_.doc.doc)
}

case class CouchAttachment(content_type: String,
                           revpos: Int,
                           digest: String,
                           data: String = "",
                           length: Int = -1,
                           stub: Boolean = false) {
  private val decoder = new BASE64Decoder()

  def toBytes: Array[Byte] = decoder.decodeBuffer(data)
}

case class CouchView(map: String, reduce: String = "")

case class CouchDesign(name: String,
                       _id: String = "",
                       _rev: String = "",
                       language: String = "javascript",
                       validate_doc_update: String = "",
                       views: Map[String, CouchView] = Map.empty[String, CouchView],
                       shows: Map[String, String] = Map.empty[String, String],
                       lists: Map[String, String] = Map.empty[String, String],
                       _attachments: Map[String, CouchAttachment] = Map.empty[String, CouchAttachment],
                       signatures: Map[String, String] = Map.empty[String, String])

case class CouchException[T](content: T) extends Throwable {
  override def toString: String = "CouchException: " + content
}

