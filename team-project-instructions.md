---
title: "Team Project"
output: 
  html_document: 
    keep_md: yes
---

The order of presentations will be randomly assigned for each section's final exam time period:

- Section 01 [T/R]: Thursday, December 12, 8 - 9:50 am
- Section 02 [M]: Monday, December 9, 6 - 8:50 pm

Written reports are due by 11:59 pm on Thursday, December 12.

### Overview

The final project for this class consists of an analysis on a data set of you and your Team Members choosing.
The goal of this project is for you to demonstrate proficiency in the techniques we have covered in this class (and beyond, if you choose) and apply them in a meaningful way.

You will be submitting two items: [1] a GitHub repo with a descriptive README.md that contains direct links to your final report (`.md` file) and presentation and [2] a Reflection on Bb that also includes a Team Peer Evaluation.

#### Getting started

GitHub Classroom will automatically make a repository for you, called `team-project-your_github_team_name`, for you to work on your assignment.
Have one Team Member follow these instructions to get the repo:

1. Sign-in to [Bb](https://mybb.gvsu.edu)
2. Go to the **Team Project** assignment page.

Here, you will find a URL leading to a page on GitHub Classroom.
Visit this page and follow the instructions.
When you obtain your repository, it should contain a copy of this `team-project-instructions.md` file and a blank `README.md` file.

I know some of you may have already created your own Team repos, but this is to avoid some sharing issues that may arise.
You can move your previous materials into this one.
At the end of the semester, I will make these Team Projects publicly available so you can share them with intern opportunities or potential employers.

##### Suggestions

- You are working within the same repo as your Team Members. Merge conflicts will happen, issues will arise, and that is fine. Commit and push often. Consider making your own branch/directory to work independently, then do a pull request. Ask questions when stuck.
- Review the grading guidelines and ask questions if any expectations are unclear.
- Make sure each member is contributing, both in quality and quantity.  I will be reviewing commits from different Team Members to verify your evaluations. Anyone judged not to have sufficiently contributed will have their final mark penalized. While Team Members have different backgrounds and abilities, it is the responsibility of every Team Member to understand how and why all code and approaches in the assignment work.
- Set aside time to work together and apart.
- When you are done, review your final report (`.md` file) on GitHub to make sure you are happy with its final state, then celebrate!

#### Evaluation

Your midterm project report and presentation will be evaluated using the [Midterm and Team Project Grading Standards](https://sta518.github.io/syllabus/assessment/#midterm-and-team-projects).
During your presentation, each Team Member should say something substantial.
Lack of substantial participation from all members will result in lower marks.

Your Reflection and Team Peer Evaluation will be evaluated using the [Meeting Preparation Grading Standards](https://sta518.github.io/syllabus/assessment/#meeting-preparation-1).
If your either your reflection or Team Peer Evaluation earns an Unsatisfactory (U), your overall mark will be lowered one mark.
Completing the Team Peer Evaluation is a prerequisite for receiving marks on the evaluation.

Due to the end of the semester time constraint, no late work or revisions will be allowed.

### Project Components

#### Report

Your write up and all typesetting must be done using R Markdown.
Each section in your report should be no more than 1 page (excluding figures) - you can check the length with a "print preview". Pay attention to your write-up presentation - neatness, coherency, and clarity will count.

Your report should at least include:

1. **Introduction**: provide an overview of your general research question and your data (where it came from, how it was obtained, what are the cases, what are the variables, etc.).
2. **Data analysis plan**: how do you plan to use your data to answer your research question? If you are using methods that we have not discussed in class, you should provide a brief description of this new method.
3. **Discussion**: showcase how you arrived to an answer regarding your research question. What have you learned about your research question and what statistical arguments can you make to support your conclusions? You should not provide an exhaustive data analysis (i.e., calculating every statistic or data visualization that we have covered for every variable), but rather show me you are proficient in using R and that you are proficient in presenting and interpreting results. A single high quality visualization would receive higher consideration than a large number of poor quality visualizations.
4. **Conclusion**: Critique your own methods and provide realistic suggestions for improving your analysis. Issues pertaining to the reliability and validity of your data and appropriateness of the statistical analysis should be discussed here.  Provide a brief discussion on what you would do differently if you were able to start over or what you would do next if you were going to continue working on this project.

Creating a Shiny App is not a requirement of this project, but you should create compelling visualizations of your data in R. There is no limit on what tools or packages you may use, but you are required to use the packages that we covered in class (i.e., the `tidyverse`).

You are free to include additional sections or name your sections differently than I have listed as you see fit, but it should be clear what your Introduction and Conclusion sections are.

Before you finalize your write-up, make sure that your chunks are turned off with `echo = FALSE`. This can be done with a global setup chunk at the start of your project:

````markdown
```{r, setup, include = FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
````

#### Presentation

Each Team will have 15 minutes maximum and each Team Member should say something substantial.

You are free to use any software you like for you final presentation, including R Markdown to create your slides (e.g., [xaringan Presentations](https://bookdown.org/yihui/rmarkdown/xaringan.html)). There is no limit on the number of slides you can use, just a time limit (again, 15 minutes). Your presentation should not dictact everything you tried ("we did this, then this, then ..."), but instead should convey what choices you made, why, and what you found.



#### Reflection

After you have presented go back to the **Team Project** assignment page on [Bb](https://mybb.gvsu.edu).
Each Team Member will individually submit a reflection here that minimally includes:

- A *clickable* link to your Team Project repository.
- A reflection on what was hard/easy, problems you came across and how you solved them, helpful tutorials you read, etc.
- Evaluation for each Team Member's contribution (yourself included) on either "Poor", "Okay", or "Great", then provide rational/explanation for your rating. I will anonymize and summarize these for each member. Provide this evaluation as a table with the following formatting:

| Team Member | Rating                     | Explination |
|:------------|:---------------------------|:------------|
| Member 1    | Good/Okay/Poor             | Specific details for Member 1 |
| Member 2    | Good/Okay/Poor             | Specific details for Member 2 |
| ...         | ...                        | ... |
| Yourself    | Good/Okay/Poor             | Specific details for yourself |
