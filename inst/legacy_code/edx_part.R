# comment and transfer to edxr.R...



# get text from version
GetText <- function(version) {
  text <- XML::xmlValue(version[["data"]][["text"]])
  return(text)
}
GetText(v_num)

# get question-level explanation from a version
GetQExplanation <- function(version) {
  question_explanation <- XML::xmlValue(version[["data"]][["explanation"]])
  return(question_explanation)
}
GetQExplanation(v_num)

# get answer from numeric question (we assume it's unique!!!)
GetNumAnswer <- function(version) {
  option <- GetOption(version, 1, 1)
  answer <- XML::xmlValue(option[["text"]])
  return(answer)
}

# get point explanation from numeric question
GetNumPointExplanation <- function(version) {
  option <- GetOption(version, 1, 1)
  point_explanation <- XML::xmlValue(option[["explanation"]])
  return(point_explanation)
}

# remove <br> tags (edx does not support them)
# replace each <br> with <p> ... </p>
BRtagToPtag <- function(output_string) {
  output_string <- paste0("<p>", output_string, "</p>")
  output_string <- stringr::str_replace_all(output_string, "<br>", "</p>\n<p>")
  return(output_string)
}

ChildNames(v_num)
v_num[["data"]] %>% ChildNames()

question_explanation <- GetQExplanation(v_num)
answer <- GetNumAnswer(v_num)
point_explanation <- GetNumPointExplanation(v_num)

# this function converts inline equations
# $$x^2$$ to \(x^2\)
DollarsToBrackets <- function(output_string) {
  bracket_pairs <- stringr::str_count(output_string, pattern = "[$][$]") / 2
  for (index in 1:bracket_pairs) {
    output_string <- stringr::str_replace(output_string,
                                          pattern = "[$][$]", replacement = "\\\\(")
    output_string <- stringr::str_replace(output_string,
                                          pattern = "[$][$]", replacement = "\\\\)")
  }
  return(output_string)
}

# abandoned function. should make markdown questions
Version2edx <- function(version) {
  text <-  DollarsToBrackets(GetText(version))
  output <- paste0(">>", text, "<<\n")
  # ... to much to do :)
  return(output)
}
cat(Version2edx(v_num))
a <- Version2edx(v_num)
a


cat(DollarsToBrackets(a))

# create edx xml of numeric question from prepared nodes
CreateNumFromNodes <- function(
  text_nodes,
  point_explanation_nodes,
  answer,
  question_explanation_nodes,
  hints_nodes_list = NULL) {

  response_node <- XML::xmlNode("numericalresponse",
                                attrs = c(answer = answer),
                                xmlNode("formulaequationinput"),
                                xmlNode("correcthint",
                                        .children = point_explanation_nodes))

  solution_node <- XML::xmlNode("solution",
                                xmlNode("div",
                    attrs = c(class = "detailed-solution"),
                  .children = question_explanation_nodes))


  problem_node <- XML::xmlNode("problem", .children = text_nodes)
  problem_node <- XML::addChildren(problem_node, response_node, solution_node)

  if (!is.null(hints_nodes_list)) {
    demandhint_node <- XML::xmlNode("demandhint")
    for (i in 1:length(hints)) {
      demandhint_node <- XML::addChildren(demandhint_node,
                                          XML::xmlNode("hint",
                                                       .children = hints_nodes_list[[i]]))
    }
    problem_node <- XML::addChildren(problem_node, demandhint_node)
  }
  return(problem_node)
}



# create edx xml of numeric question from plain text
CreateNumFromText <- function(
  text = "Кому на Руси жить хорошо?",
  point_explanation = "here is the hint",
  answer = 4,
  question_explanation = "Тому, кто досмерти работает, до полусмерти пьет",
  hints = c("hint раз", "hint 2")) {

  # transfrom plain text to xml <p> nodes taking care of <br> tag
  text_nodes <- TextToNodes(text)
  question_explanation_nodes <- TextToNodes(question_explanation)
  point_explanation_nodes <- TextToNodes(point_explanation)

  hints_nodes_list <- TextToNodes(hints)


  problem_node <- CreateNumFromNodes(text_nodes = text_nodes,
                                     point_explanation_nodes = point_explanation_nodes,
                                     answer = answer,
                                     question_explanation_nodes = question_explanation_nodes,
                                     hints_nodes_list = hints_nodes_list)


  return(problem_node)
}

# summary on questions
QuestionSummary <- function(doc) {
  n_questions <- GetNumberOfQuestions(doc)
  q_summary <- data_frame(question_no = 1:n_questions, n_versions = NA)
  for (i in 1:n_questions) {
    q_summary$n_versions[i] <- GetNumberOfVersions(doc, i)
  }
  return(q_summary)
}
QuestionSummary(doc)


GetNumberOfOptionGroups <- function(version) {
  n_option_groups <- XML::xmlSize(
    version[["data"]][["option_groups"]])
  return(n_option_groups)
}

# summary on versions of questions
VersionSummary <- function(doc) {
  q_summary <- QuestionSummary(doc)
  v_summary <- data_frame(
    question_no = rep(q_summary$question_no,
                      times = q_summary$n_versions))
  v_summary <- v_summary %>% group_by(question_no) %>%
    mutate(version_no = row_number())
  v_summary <- ungroup(v_summary) %>%
    mutate(type = NA, n_option_groups = NA)
  tot_n_versions <- nrow(v_summary)

  for (i in 1:tot_n_versions) {
    version <- GetVersion(doc,
                          v_summary$question_no[i],
                          v_summary$version_no[i])
    v_summary$type[i] <- GetType(version)
    v_summary$n_option_groups[i] <- GetNumberOfOptionGroups(version)
  }

  return(v_summary)
}



# summary on option groups
OptionGroupSummary <- function(doc) {
  v_summary <- VersionSummary(doc)
  tot_n_versions <- nrow(v_summary)
  indexes <- rep(1:tot_n_versions, v_summary$n_option_groups)
  og_summary <- v_summary[indexes, ]
  og_summary <- group_by(og_summary, question_no, version_no) %>%
    mutate(option_group_no = row_number()) %>% ungroup()
  og_summary <- mutate(og_summary, n_options = NA, select = NA)

  for (i in 1:nrow(og_summary)) {
    version <- GetVersion(doc,
                          og_summary$question_no[i],
                          og_summary$version_no[i])
    option_group <- GetOptionGroup(version,
                                   og_summary$option_group_no[i])
    og_summary$n_options[i] <- XML::xmlSize(option_group)
    og_summary$select[i] <- XML::xmlAttrs(option_group, "select")
  }

  og_summary <- mutate(og_summary,
                       select_num = as.numeric(ifelse(select == "all", n_options, select)),
                       cardinality = choose(n = n_options, k = select_num) )

  return(og_summary)
}

GetTotalNumberOfOptions <- function(version) {
  n_option_groups <- GetNumberOfOptionGroups(version)
  tot_n_options <- 0
  for (i in 1:n_option_groups) {
    option_group <- GetOptionGroup(version, i)
    tot_n_options <- tot_n_options + XML::xmlSize(option_group)
  }
  return(tot_n_options)
}

GetTotalNumberOfOptions(v_radio)
GetTotalNumberOfOptions(v_num)
GetTotalNumberOfOptions(v_checkbox)

og <- OptionGroupSummary(doc)
v <- VersionSummary(doc)




GetNumberOfOptionGroups(v_radio)


# text may have <br> <p> </p> tags
# edx does not support <br> tag
# TextToNodes0 transforms all text between <br> into separate <p> xml nodes
# to avoid possible nesting of <p> ... </p> with existing ones
# TextToNodes0 transforms all </p> to <br> and removes all <p> beforehand
# replace_dollars option --- transform $$...$$ to \(...\)
TextToNodes0 <- function(input_text, node_name = "p", replace_dollars = TRUE) {
  if (is.null(input_text)) {
    return(NULL)
  }

  if (replace_dollars) {
    input_text <- DollarsToBrackets(input_text)
  }

  # remove <p>
  output_text <- stringr::str_replace_all(input_text, "<p>", "")
  # replace </p> with <br>
  output_text <- stringr::str_replace_all(output_text, "</p>", "<br>")

  fragments <- stringr::str_split(output_text, "<br>")[[1]]

  # add <p> ... </p>
  fragments <- paste0("<p>", fragments, "</p>")

  list_of_nodes <- list()
  for (i in 1:length(fragments)) {
    parsed_fragment <- XML::xmlTreeParse(file = fragments[i],
                                         asText = TRUE, isHTML = TRUE)
    list_of_nodes[[i]] <- XML::xmlRoot(parsed_fragment)
  }

  return(list_of_nodes)
}

# the same as TextToNodes0, but vectorized
# if vector_input is of length 1 returns a list of nodes
# if vector_input is of length > 1 returns a list of list of nodes
TextToNodes <- function(vector_input) {
  if (length(vector_input) < 2) { # 0 (if NULL) and 1 are ok
    nodes_list <- TextToNodes0(vector_input)
  } else {
    nodes_list <- list()
    for (i in 1:length(vector_input)) {
      nodes_list[[i]] <- TextToNodes0(vector_input[i])
    }
  }

  return(nodes_list)
}

#
TransformNumeric <- function(version) {


  # Check
  type <- GetType(version)
  n_og <- GetNumberOfOptionGroups(version)
  tot_n_options <- GetTotalNumberOfOptions(version)
  if (!type == "numeric") {
    warning("The question should be numeric, but type = ", type)
  }
  if (n_og > 1) {
    warning("The question should have one option group, but has = ", n_og)
  }
  if (tot_n_options > 1) {
    warning("The question should have one option in total, but has = ", tot_n_options)
  }

  # obtain plain text
  text <- GetText(version)
  question_explanation <- GetQExplanation(version)

  answer <- GetNumAnswer(version)
  point_explanation <- GetNumPointExplanation(version)

  hints <- NULL


  # transfrom plain text to xml <p> nodes taking care of <br> tag
  text_nodes <- TextToNodes(text)
  question_explanation_nodes <- TextToNodes(question_explanation)
  point_explanation_nodes <- TextToNodes(point_explanation)

  hints_nodes_list <- TextToNodes(hints)


  edx_question <- CreateNumFromNodes(text_nodes = text_nodes,
                                     point_explanation_nodes = point_explanation_nodes,
                                     answer = answer,
                                     question_explanation_nodes = question_explanation_nodes,
                                     hints_nodes_list = hints_nodes_list)
  return(edx_question)
}

TransformNumeric(v_num)


Coursera2edx <- function(doc, wd = getwd()) {

  message("Questions saved in folder: ", wd)

  n_questions <- GetNumberOfQuestions(doc)
  test_title <- GetTitle(doc)
  test_preamble <- GetPreamble(doc)

  message("Test: ", test_title)
  message("Number of questions: ", n_questions)

  for (q_no in 1:n_questions) {
    n_versions <- GetNumberOfVersions(doc, question_no = q_no)
    message("Processing question ", q_no,". Number of versions: ", n_versions)
    for (v_no in 1:n_versions) {
      version <- GetVersion(doc, q_no, v_no)
      v_type <- GetType(version)

      message("Question: ", q_no, ". Version: ", v_no, ". Type: ", v_type, ".")
    }
  }
}

a <- GetText(GetVersion(doc, 17, 1))
a

og_summary <- OptionGroupSummary(doc)
q <- GetVersion(doc, 20, 2)
# TransformNumeric(q)

# img tag 13.2, 12.2, 14.2 i/b mismatch 15.2, 16


# in our econometrics xml we have wrong <i><b>...</i></b> tags
# correct them to <b><i>...</i></b>
CorrectIBtags <- function(filename) {
  all <- paste(readLines(filename), collapse = "\n")
  all <- stringr::str_replace_all(all,
                         pattern = "<i> *<b>",
                         replacement = "<b><i>")
  all <- stringr::str_replace_all(all,
                         pattern = "</b> *</i>",
                         replacement = "</i></b>")
  new_filename <- paste0(
    stringr::str_sub(filename, end = -5), "_ibcorr.xml")
  writeLines(all, con = new_filename)
}




