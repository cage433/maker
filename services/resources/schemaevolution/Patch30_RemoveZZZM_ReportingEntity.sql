delete
  FROM Portfolio
  where portfolio like 'ZZZ%'

delete VarReport from  VarReport vr
inner join VarReportheader vrh on vr.varreportheaderid = vrh.id
where vrh.reportingEntityName like 'ZZZ%'

delete from VarReportheader
where reportingEntityName like 'ZZZ%'