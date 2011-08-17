in_namespace('EDM') {
   in_namespace('Trades') {

     define('Transaction') {
       constant 'commentLength', 4000
       field 'oid',                             :integer, :identifier => true
       field 'book',                            :guid
       field 'comments',                        :string, :max_length => Transaction::COMMENTLENGTH
     }

    define('Execution') {
      field 'oid',                              :integer, :identifier => true
    }
  }
}
