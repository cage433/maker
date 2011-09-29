in_namespace('Internal.Logistics.Movement') {

  define('LocalDateTime') {
	field 'date',		   	:date
	field 'millisFromMidnight',	:integer, :optional => true
  }

  define('BillOfLading') {
	field 'billOfLadingId',		:integer
	field 'blNumber',		:string
        field 'quantity',		'TradeMgmt.Internal.RefinedMetal.Quantity'
	field 'blDate',			:date
	field 'originalReceived',	:boolean
  }

  define('TruckDetails') {
	field 'truckId',		:integer
        field 'truckReference',		:string, :optional => true
	field 'truckBillOfLading',	:string, :optional => true
	field 'carrierId',		:guid, :optional => true        # :references => 'Counterparty(oid)
	field 'bundles',		:integer, :optional => true
	field 'grossWeight', 		'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true
	field 'netWeight',		'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true
	field 'departDate',		:'LocalDateTime', :optional => true
	field 'departDateTimeStatus',   :enum, :enumerated => ['ESTIMATED', 'ACTUAL']
	field 'arriveDate',		:'LocalDateTime', :optional => true
    	field 'arriveDateTimeStatus',   :enum, :enumerated => ['ESTIMATED', 'ACTUAL']
	field 'removed',		:boolean
	field 'nullDiscriminator',	:integer
  }

  define('RailcarDetails') {
	field 'railcarId',		:integer
        field 'wagonReference',		:string, :optional => true
	field 'wagonBillOfLading',	:string, :optional => true
	field 'bundles',		:integer, :optional => true
	field 'grossWeight', 		'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true
	field 'netWeight',		'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true
	field 'departDate',		:'LocalDateTime', :optional => true
	field 'departDateTimeStatus',   :enum, :enumerated => ['ESTIMATED', 'ACTUAL']
	field 'arriveDate',		:'LocalDateTime', :optional => true
    	field 'arriveDateTimeStatus',   :enum, :enumerated => ['ESTIMATED', 'ACTUAL']
	field 'removed',		:boolean
	field 'nullDiscriminator',	:integer
  }

  define('TransportDetails', :abstract => true) {
	field 'transportDetailsId',	:integer
  }

  define('ShipTransportDetails', :extends => 'TransportDetails') {
	field 'carrierId',		:guid, :optional => true        # :references => 'Counterparty(oid)
	field 'carrierReference',	:string, :optional => true
	#field 'vessel',		:guid, :optional => true	# :references => 'Vessel(oid)
	field 'laycanStart',		'LocalDateTime', :optional => true
	field 'laycanEnd',		'LocalDateTime', :optional => true
	field 'billsOfLading',		:list, :element_type => 'BillOfLading'
  }

  define('RoadTransportDetails', :extends => 'TransportDetails') {	
	field 'carrierId',		:guid, :optional => true        # :references => 'Counterparty(oid)
	field 'carrierReference',	:string, :optional => true
        field 'truckReference',		:string, :optional => true
	field 'numberOfTrucks',		:integer, :optional => true
	field 'trucks',			:list, :element_type => 'TruckDetails' 
  }

  define('RailTransportDetails', :extends => 'TransportDetails') { 
	field 'carrierId',		:guid, :optional => true        # :references => 'Counterparty(oid)
	field 'carrierReference',	:string, :optional => true
	field 'numberOfRailcars',	:integer, :optional => true
	field 'railcars',		:list, :element_type => 'RailcarDetails' 
  }

  define('MovementEvent') {
    field 'eventId',                    :integer
    field 'eventType',                  :enum, :enumerated => ['DEPART', 'ARRIVE']
    field 'eventDate',                  'LocalDateTime'
    field 'locationId',                 :guid        # :references => 'LogisticsLocation(oid)'
    field 'eventDateTimeStatus',        :enum, :enumerated => ['ESTIMATED', 'ACTUAL']
  }

  define('Movement', :abstract => true) {
    field 'movementId',                 :integer
    field 'audit',                      'Audit'
    field 'trafficOperatorId',          :integer        # :references => 'User(oid)'
    field 'status',                     :enum, :enumerated => ['PLANNED', 'INTRANSIT', 'ARRIVED', 'COMPLETE', 'CANCELLED']
    field 'quantity',                   'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true
    field 'agentId',                    :guid, :optional => true        # :references => 'Counterparty(oid)'
    field 'agentReference',             :string, :optional => true
    field 'comments',                   :string, :optional => true
    field 'autoSumInventory',           :boolean
  }

  define('MovementSummary', :extends => 'Movement') {
    field 'transportModeType',          :string, :optional => true
    field 'origin',                     'MovementEvent', :optional => true
    field 'destination',                'MovementEvent', :optional => true
  }

  define('MovementDetail', :extends => 'Movement') {
    field 'events',                     :list, :element_type => 'MovementEvent'
    field 'transportDetails',           'TransportDetails', :optional => true
  }

  define('MovementResponse', :extends => 'LogisticsResponse') {
    field 'affectedMovementSummary',    'MovementSummary', :optional => true
  }
}

