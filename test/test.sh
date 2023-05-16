#!/bin/sh

curl -v    http://localhost:8888/getvars > vars
curl -v   'http://localhost:8888/stats?variable=Alanine.aminotransferase..Enzymatic.activity.volume..in.Serum.or.Plasma&cohort=accelerate.db' >stats
