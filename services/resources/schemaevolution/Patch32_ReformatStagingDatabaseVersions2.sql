alter table dbo.ForwardCurveStaging
add constraint [IX_ForwardCurveStaging] UNIQUE NONCLUSTERED (
	[observationDate] ASC,
	[pricingGroupID] ASC,
	[subgroupName] ASC,
	[curveID] ASC,
	[version] ASC
);
