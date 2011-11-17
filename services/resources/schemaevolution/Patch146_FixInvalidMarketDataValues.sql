ALTER TABLE [dbo].[MarketDataExtendedKey]
  ADD CONSTRAINT PK_MarketDataExtendedKey PRIMARY KEY (id)

ALTER TABLE [dbo].[MarketDataValueKey]
  ADD CONSTRAINT PK_MarketDataValueKey PRIMARY KEY (id)

ALTER TABLE [dbo].[MarketDataValue]
  ADD CONSTRAINT FK_ExtendedKey FOREIGN KEY (extendedKey) REFERENCES [dbo].[MarketDataExtendedKey] (id)

ALTER TABLE [dbo].[MarketDataValue]
  ADD CONSTRAINT FK_ValueKey FOREIGN KEY (valueKey) REFERENCES [dbo].[MarketDataValueKey] (id)