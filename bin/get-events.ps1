[void][Reflection.Assembly]::LoadFile("C:\Program Files\Microsoft\Exchange\Web Services\2.0\Microsoft.Exchange.WebServices.dll")

$service = new-object Microsoft.Exchange.WebServices.Data.ExchangeService([Microsoft.Exchange.WebServices.Data.ExchangeVersion]::Exchange2007_SP1)
$service.Url = new-object Uri("https://mymail.qualcomm.com/EWS/Exchange.asmx")

$attendees = new-object System.Collections.ObjectModel.Collection[Microsoft.Exchange.WebServices.Data.AttendeeInfo]
$attendee = new-object Microsoft.Exchange.WebServices.Data.AttendeeInfo([Environment]::UserName + "@qti.qualcomm.com")
$attendees.Add($attendee)

$start = [DateTime]::Parse([DateTime]::Now.ToString("yyyy-MM-dd 0:00"))
$end = $start.AddDays(1)

$duration = new-object Microsoft.Exchange.WebServices.Data.TimeWindow($start, $end)

$availability = $service.GetUserAvailability($attendees, $duration, [Microsoft.Exchange.WebServices.Data.AvailabilityData]::FreeBusy)

$events = @()
foreach ($event in $availability.AttendeesAvailability[0].CalendarEvents) {
    $events += (new-object PSObject |
        add-member -PassThru NoteProperty from $event.StartTime |
        add-member -PassThru NoteProperty to $event.EndTime)
}

$json = convertto-json $events
echo $json

