package other

object GithubCommits {

  def main(args: Array[String]): Unit = {
    val repoOwner = "hmrc"
    val repo = "vat-summary-frontend"
    val branch = "master"
    val url = s"https://github.com/$repoOwner/$repo/commits/$branch.atom"
    val source = io.Source.fromURL(url)
    val xml = source.getLines.map(_.trim).toList
    val rawCommits = getCommits(xml)

    cleanCommits(rawCommits, repo, branch)
  }

  def getCommits(list: List[String], newList: List[String] = List()): List[String] = list.headOption match {
    case None => newList
    case Some("<entry>") => getCommits(list.tail, newList :+ list.splitAt(list.indexOf("</entry>"))._1.mkString)
    case _ => getCommits(list.tail, newList)
  }

  def cleanCommits(list: List[String], repo: String, branch: String): Unit = {
    val userRegex = """<name>(.*)</name>""".r
    val dateRegex = """<updated>(.*)</updated>""".r
    val commitRegex = """<title>(.*)</title>""".r
    println(s"Repo: $repo, Branch: $branch")
    println("------------------------------------------")
    for(item <- list) {
      val user = userRegex.findAllIn(item).group(1)
      val date = dateRegex.findAllIn(item).group(1).take(10)
      val commit = commitRegex.findAllIn(item).group(1).replace("&#39;", "'")
      println(s"User: $user, Date: $date, Commit: $commit")
    }
  }
}
