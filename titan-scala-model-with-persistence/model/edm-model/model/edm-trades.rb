
#EDM for trades
in_namespace('EDM') {
  in_namespace('Trades') {

    define('ContractualAgreement') {
      field 'oid',                              :integer, :identifier => true
      field 'versionId',                        :string
    }

    define('TradeSpec') {
      field 'oid',                              :integer, :identifier => true
    }

    define('TradeContractTerms') {
      field 'oid',                              :integer, :identifier => true
      field 'contractDate',                        :date
      field 'generalTermsAndConditions',        :string
      field 'notes',                            :string
      field 'tradeSpec',                        :'TradeSpec'
    }

    define('PhysicalTradeContractTerms', :extends => 'TradeContractTerms') {
      field 'inspShareAtLP',                    :'Percentage' ## inspectionShareAtLoadPort should be the name and inspShareAtLP the label
      field 'inspShareAtDP',                    :'Percentage' ## inspectionShareAtDischargePort should be the name and inspShareAtDP the label
    }

    define('CounterpartyAndTypeHolder') {
      field 'counterparty',                   :guid
      field 'nonKYC',                         :boolean
    }

    define('Trade', :extends => 'Transaction') {
      constant 'PURCHASE', 'P'
      constant 'SALE', 'S'
      field 'contractHandler',                  :integer_key, :optional => true, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
      field 'groupCompany',                     :guid, :optional => true
      field 'tstate',                            :enum, :enumerated => ['Scratch', 'Completed', 'Cancelled', 'Completing'] 
      field 'originatingOffice',                :guid
      field 'analyst',                          :integer_key, :optional => true, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
      field 'trader',                           :integer_key, :optional => true, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
      field 'submitter',                        :integer_key, :optional => true, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
      field 'operator',                         :integer_key, :optional => true, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
      field 'direction',                        :string

      field 'contractDraftedDate',              :date
      field 'contractReceivedDate',             :date
      field 'brokerContractReceivedDate',       :date
      field 'responseSentDate',                 :date
      field 'contractSentDate',                 :date
      field 'submitted',                        :datetime
      field 'contractFinalised',                :boolean
      field 'contractComment',                  :string, :max_length => Transaction::COMMENTLENGTH
      field 'contractualAgreement',             :'ContractualAgreement'
      field 'tradeContractTerms',               :'TradeContractTerms'
	  field 'tradeId',							:integer
    }

    define('Contact') {
      field 'oid',                              :integer, :identifier => true
    }

    define('OTCTrade', :extends => 'Trade') {
      field 'counterparty',                     'CounterpartyAndTypeHolder'
      field 'counterpartyContact',              :integer_key, :optional => true, :references => 'EDM.Trades.Contact(oid)'
    }
  }
}
