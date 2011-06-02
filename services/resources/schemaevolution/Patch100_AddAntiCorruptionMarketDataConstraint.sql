ALTER TABLE [dbo].[MarketData] ADD CONSTRAINT
  IX_CORRUPT_MDS UNIQUE (observationDay, observationTime, marketDataSet, marketDataType, marketDataKey, childVersion)