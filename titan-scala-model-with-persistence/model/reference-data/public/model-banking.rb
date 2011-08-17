# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Banking
in_namespace('commondomains.referencedata.masterdata.banking') {
=begin
  A bank account belonging to a Trafigura group company.
  TODO: find out do we have to use this entity or just BankAccount
=end
  define('GroupBankAccount', :extends => 'BankAccount') {
    #Used in securitisation.
    #TODO: findout how to map this field
    field 'templateCode',      :string

    #The business line for which the bank account is used.
    #TODO: findout how to map this field
    # field 'businessLine',      'BusinessLine'
  }
}