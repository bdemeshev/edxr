# read coursera test xml and convert it to markdown/xml edx (openedu)



#' Obtain title of a test from coursera xml
#'
#' Obtain title of a test from coursera xml
#'
#' Obtain title of a test from coursera xml
#'
#' @param doc parsed xml tree, obtained by XML::xmlTreeParse
#' @return character test title
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' GetTitle(doc)
GetTitle = function(doc) {
  root <- XML::xmlRoot(doc)
  test_name <- XML::xmlValue(
    root[["metadata"]][["title"]][[1]])
  return(test_name)
}


#' Obtain preamble of a test from coursera xml
#'
#' Obtain preamble of a test from coursera xml
#'
#' Obtain preamble of a test from coursera xml
#'
#' @param doc parsed xml tree, obtained by XML::xmlTreeParse
#' @return character test title
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' GetPreamble(doc)
GetPreamble = function(doc) {
  root <- XML::xmlRoot(doc)
  preamble <- XML::xmlValue(root[["preamble"]])
  return(preamble)
}




#' obtain number of a questions from coursera xml
#'
#' obtain number of a questions from coursera xml
#'
#' obtain number of a questions from coursera xml
#'
#' @param doc parsed xml tree, obtained by XML::xmlTreeParse
#' @return character test title
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' GetNumberOfQuestions(doc)
GetNumberOfQuestions <- function(doc) {
  root <- XML::xmlRoot(doc)
  number_of_questions <-
    XML::xmlSize(root[["data"]][["question_groups"]])
  return(number_of_questions)
}




#' obtain all versions of a question from coursera xml
#'
#' obtain all versions of a question from coursera xml
#'
#' obtain all versions of a question from coursera xml
#'
#' @param doc parsed xml tree, obtained by XML::xmlTreeParse
#' @param question_no int number of question
#' @return xml tree with question
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' q_17 <- GetQuestion(doc, 17)
GetQuestion <- function(doc, question_no) {
  root <- XML::xmlRoot(doc)
  number_of_questions <- GetNumberOfQuestions(doc)
  if (question_no > number_of_questions) {
    stop("Question number ", question_no, " requested, but total number of question is ",
         number_of_questions)
  }
  return(root[["data"]][["question_groups"]][[question_no]])
}


#' obtain all versions of a question from coursera xml
#'
#' obtain all versions of a question from coursera xml
#'
#' obtain all versions of a question from coursera xml
#'
#' @param doc parsed xml tree, obtained by XML::xmlTreeParse
#' @param question_no int number of question
#' @return int number of versions
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' GetNumberOfVersions(doc, 17)
GetNumberOfVersions <- function(doc, question_no) {
  question <- GetQuestion(doc, question_no)
  n_versions <- XML::xmlSize(question) - 1
  return(n_versions)
}




#' obtain specific version of a question from coursera xml
#'
#' obtain specific version of a question from coursera xml
#'
#' obtain specific version of a question from coursera xml
#'
#' @param doc parsed xml tree, obtained by XML::xmlTreeParse
#' @param question_no int number of question
#' @param version_no int number of version
#' @return xml tree with version of question
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' v_num <- GetVersion(doc, 10, 1)
#' v_checkbox <- GetVersion(doc, 4, 1)
#' v_radio <- GetVersion(doc, 3, 1)
GetVersion <- function(doc, question_no, version_no) {
  question <- GetQuestion(doc, question_no)
  version <- question[[1 + version_no]]
  return(version)
}







#' get option group from version
#'
#' get option group from version
#'
#' get option group from version
#'
#' @param version xml tree of a version
#' @param option_group_no int number of option group
#' @return xml tree with version of option group
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' q_06_1 <- GetVersion(doc, 6, 1)
#' og <- GetOptionGroup(q_06_1, 1)
GetOptionGroup <- function(version, option_group_no) {
  option_group <- version[["data"]][["option_groups"]][[option_group_no]]
  return(option_group)
}


#' get option from version
#'
#' get option from version
#'
#' get option from version
#'
#' @param version xml tree of a version
#' @param option_group_no int number of option group
#' @param option_no int number of option
#' @return xml tree with version of option group
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' q_06_1 <- GetVersion(doc, 6, 1)
#' option <- GetOption(q_06_1, 1, 1)
GetOption <- function(version, option_group_no, option_no) {
  option_group <- GetOptionGroup(version, option_group_no)
  option <- option_group[[option_no]]
  return(option)
}


#' get child names of an xml node
#'
#' get child names of an xml node
#'
#' get child names of an xml node
#'
#' @param node xml tree node
#' @return character vector of child names
#' @export
#' @examples
#' test_01_path <- system.file("extdata", "week_01_test_01_ibcorr.xml", package = "edxr")
#' doc <- XML::xmlTreeParse(test_01_path)
#' v_num <- GetVersion(doc, 10, 1)
#' ChildNames(v_num)
ChildNames <- function(node) {
  return(XML::xmlSApply(node, XML::xmlName))
}

