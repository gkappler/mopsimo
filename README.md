mopsimo
=======

Ihr müsst nur noch in eurem lokalen Clone die Rohdaten in das Verzeichnis data/ kopieren. Diese werden dann nicht in das öffentliche Repository synchronisiert.

In den Skripten dann einfach den relativen Pfad angeben, z.B. in src/R/MQ_felix.R wäre das:
<code>
dat <- read.table("../../data/MQ.tab", header=TRUE)
</code>
