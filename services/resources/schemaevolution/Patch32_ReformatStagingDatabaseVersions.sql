update a
set childVersion = b.[newVersion]
from dbo.ForwardCurveStaging as a
inner join dbo.ForwardCurveStaging as b
on a.childVersion=b.version and
   a.curveID=b.curveID and
   a.pricingGroupID=b.pricingGroupID and
   a.subgroupName=b.subgroupName and
   a.observationDate=b.observationDate;

alter table dbo.ForwardCurveStaging
drop IX_ForwardCurveStaging;

alter table dbo.ForwardCurveStaging
drop column version;
