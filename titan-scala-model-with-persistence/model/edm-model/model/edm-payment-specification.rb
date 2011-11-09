in_namespace('EDM.PaymentSpecs') {

#  define('EDMPaymentMethod') {
#    field 'oid',                            :integer, :identifier => true
#  }
  
#  define('EDMTelegraphicTransfer', :extends => 'EDMPaymentMethod') {
#    field 'daysGrace',                      :integer
#  }

#  define('EDMLetterOfCredit', :extends => 'EDMPaymentMethod') {
#    field 'tolerance',                      :'Tolerance'
#  }
  
#  define('EDMOpenAccount', :extends => 'EDMPaymentMethod'){
#  }
  
#  define('EDMGuarantee') {
#    field 'amount',                         :'EDM.shared.types.Quantity'
#    field 'requiredBy',                     :date
#  }

  define('EDMPaymentMethod') {
  }
     
 define('PaymentDayHolidayRule') {
 }
 define('HolidaysCalendar') {
 }
 define('PaymentDayRule') {
 }
 define('WorkingDaysPaymentDayRule', :extends => 'PaymentDayRule') {
  field 'holidaysCalendar',               :'HolidaysCalendar'
 }
 define('CalendarDaysPaymentDayRule', :extends => 'PaymentDayRule') {
  field 'ruleIfSaturday',      :'PaymentDayHolidayRule'
  field 'ruleIfSunday',        :'PaymentDayHolidayRule'
  field 'ruleIfHolidayMonday', :'PaymentDayHolidayRule'
  field 'ruleIfOtherMonday',   :'PaymentDayHolidayRule'
 }

  define('PaymentDueDate') {
     field 'daysOffset',                    :integer
     field 'percentageToPay',               :real
     field 'paymentPeriod',                 :'PaymentDayRule'
  }

 define('InterestRule') {
 }

 define('CreditTerms') {
    field 'interestRuleTillPaymentDate',  :'EDM.PaymentSpecs.InterestRule'
    field 'interestTillPaymentDate',      :real
    field 'interestRuleAfterPaymentDate', :'EDM.PaymentSpecs.InterestRule'
    field 'interestAfterPaymentDate',     :real
    field 'maximumDays',                  :integer
 }

 define('PreArrangedCreditTerms', :extends => 'CreditTerms') {
 }

 define('ExtendedCreditTerms', :extends => 'CreditTerms') {
 }
     
  define('EDMPresentedDocument') {
    field 'oid',                            :integer, :identifier => true
    field 'documentType',                   :integer_key, :references => 'TradeCapture.Internal.RefinedMetal.DocumentType(oid)'
    field 'description',                    :string
    field 'original',                       :boolean
  }
  
  define('RiskApproval') {
    field 'user',      :string
  }

  define('EDMPaymentSpec') {
    constant 'commentLength', 4000
#    field 'oid',                               :integer, :identifier => true
    field 'paymentCurrency',                   :'EDM.shared.types.Currency'
    field 'sellerChargesRechargedToBuyer',     :boolean, :optional => true
    field 'comments',                          :string, :optional => true, :max_length => EDMPaymentSpec::COMMENTLENGTH
    field 'paymentMethods'				,            :list, :element_type => 'EDMPaymentMethod'
    field 'presentedDocuments',                :list, :element_type => 'EDMPresentedDocument'
    field 'paymentDueDates',                   :list, :element_type => 'PaymentDueDate'
    field 'riskApproval',                      :list, :element_type => 'RiskApproval'
    field 'preArrangedCreditTerms',            :'PreArrangedCreditTerms'
    field 'extendedCreditTerms',               :'ExtendedCreditTerms'
  }
}
