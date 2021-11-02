package com.pinkstack

final case class Book(title: String)

object Recommender:
  type Query = String
  type Books = Seq[Book]

  val words: String => Set[String] =
    _.split(" ").map(_.strip().toLowerCase())
      .toSet.filterNot(_.length <= 3)

  val jaccardIndex: (Query, Book) => Double = (q, book) =>
    val Seq(intersection, union) = Seq(
      words(q) intersect words(book.title),
      words(q) union words(book.title)).map(_.size.toDouble)
    1 - {
      if (union == 0) 0 else intersection / union
    }

  val recommend: (Query, Books) => Seq[(Book, Double)] = (query, books) =>
    books.map(b => (b, jaccardIndex(query, b))).sortBy(_._2)

object RecommenderApp extends App:
  val books: Array[Book] =
    """Designing Data-Intensive Applications
      |Programming Scala, 3rd Edition
      |Programming Scala
      |Practical FP in Scala: A hands-on approach
      |Scala in 8 Hours
      |Think Python: An Introduction to Software Design
      |Modern Systems Programming with Scala Native
      |Programming in Scala""".stripMargin.split('\n').map(Book.apply)

  Recommender.recommend("Scala", books)
    .take(5)
    .foreach(println)
