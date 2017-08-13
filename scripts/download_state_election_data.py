import csv
import gspread
#from oauth2client.service_account import ServiceAccountCredentials

#scope = ['https://spreadsheets.google.com/feeds']
#"https://docs.google.com/spreadsheets/d/1VfkHtzBTP5gf4jAu8tcVQgsBJ1IDvXEHjuMqYlOgYbA"
#credentials = ServiceAccountCredentials.from_json_keyfile_name('credentials.json', scope)

docid = "0zjVQXjJixf-SdGpLKnJtcmQhNjVUTk1hNTRpc0x5b9c"

client = gspread.authorize()
spreadsheet = client.open_by_key(docid)
for i, worksheet in enumerate(spreadsheet.worksheets()):
    filename = docid + '-worksheet' + str(i) + '.csv'
    with open(filename, 'wb') as f:
        writer = csv.writer(f)
        writer.writerows(worksheet.get_all_values())