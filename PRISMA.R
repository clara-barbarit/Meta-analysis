library(DiagrammeR)

plot <- grViz("
digraph prisma {

graph [
  layout = dot,
  rankdir = TB,
  splines = ortho,
  nodesep = 0.45,
  ranksep = 0.65,
  bgcolor = white
]

node [
  shape = box,
  style = 'rounded,filled',
  fontname = Helvetica,
  fontsize = 14,
  color = grey35,
  penwidth = 1.6,
  margin = 0.20
]

edge [
  color = grey25,
  penwidth = 1.3,
  arrowsize = 0.8
]

id_label [
  label = 'IDENTIFICATION',
  fillcolor = '#FCE4C4',
  color = '#E67E22',
  fontsize = 14,
  width = 1.2,
  height = 0.8
]

screen_label [
  label = 'SCREENING',
  fillcolor = '#DFF2D8',
  color = '#4C9A2A',
  fontsize = 14,
  width = 1.1,
  height = 1.7
]

inc_label [
  label = 'INCLUDED',
  fillcolor = '#DCEEFF',
  color = '#2E75B6',
  fontsize = 14,
  width = 1.0,
  height = 0.8
]

database [
  label = <
  <b>Articles identified from databases</b><br/><br/>
  - Scopus : n = 789<br/>
  - Web of Science : n = 886<br/>
  - EconLit : n = 515
  >,
  fillcolor = '#FCE4C4',
  color = '#E67E22',
  width = 4.7
]

duplicates [
  label = <
  <b>Articles excluded</b><br/>
  duplicates<br/>
  n = 567
  >,
  fillcolor = '#FCE4C4',
  color = '#E67E22',
  width = 2.8
]

titleabs [
  label = <
  <b>Articles screened by title &amp; abstract</b><br/><br/>
  n = 1623
  >,
  fillcolor = '#DFF2D8',
  color = '#4C9A2A',
  width = 4.7
]

excl_titleabs [
  label = <
  <b>Articles excluded</b><br/>
  n = ...<br/><br/>
  - Critère 1<br/>
  - Critère 2<br/>
  - Critère 3
  >,
  fillcolor = '#DFF2D8',
  color = '#4C9A2A',
  width = 3.0
]

fulltext [
  label = <
  <b>Articles screened by full text</b><br/><br/>
  n = ...
  >,
  fillcolor = '#DFF2D8',
  color = '#4C9A2A',
  width = 4.7
]

excl_fulltext [
  label = <
  <b>Articles excluded</b><br/>
  n = ...<br/><br/>
  - Critère 1<br/>
  - Critère 2<br/>
  - Critère 3
  >,
  fillcolor = '#DFF2D8',
  color = '#4C9A2A',
  width = 3.0
]

snowball [
  label = <
  <b>Articles identified<br/>by snowballing</b><br/>
  n = ...
  >,
  fillcolor = '#DFF2D8',
  color = '#4C9A2A',
  width = 2.4
]

included [
  label = <
  <b>Articles included in the meta-analysis</b><br/><br/>
  n = ...
  >,
  fillcolor = '#DCEEFF',
  color = '#2E75B6',
  width = 4.7
]

point_join [
  shape = point,
  width = 0.02,
  height = 0.02,
  label = '',
  style = invis
]

{rank = same; id_label; database; duplicates}
{rank = same; screen_label; titleabs; excl_titleabs}
{rank = same; fulltext; excl_fulltext; snowball}
{rank = same; inc_label; included}

id_label -> screen_label [style = invis]
screen_label -> inc_label [style = invis]

database -> duplicates
database -> titleabs

titleabs -> fulltext
titleabs -> excl_titleabs

fulltext -> excl_fulltext
fulltext -> point_join
snowball -> point_join
point_join -> included

}
")

plot