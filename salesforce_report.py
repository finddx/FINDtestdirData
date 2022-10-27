from simple_salesforce import Salesforce
import requests
import os
import sys

reports = {
	"00O6900000CNcZQEA1": "testdir",
	"00O6900000BnnLZEAZ": "chagas_testdir",
	"00O6900000CGGvtEAH": "mpx_testdir",
	"00O6900000CGJeXEAX": "ebov_testdir",
	"00O6900000CGKWKEA5": "eqa_covid_testdir"
}

if len(sys.argv) < 2:
	print("Error: Please enter the report ID")
	exit(1)

# Sign into Salesforce
sf = Salesforce(username=os.getenv('SF_USER'), password=os.getenv('SF_PASS'), security_token=os.getenv('SF_TOKEN'))

# Set report details
sf_org = 'https://find.my.salesforce.com/'
export_params = '?isdtp=p1&export=1&enc=UTF-8&xf=csv'

# Download report
for report_id in sys.argv[1:]:
	sf_report_url = sf_org + report_id + export_params
	response = requests.get(sf_report_url, headers=sf.headers, cookies={'sid': sf.session_id})
	if response.status_code != 200:
		print("Error: report not found")
		exit(1)
	new_report = response.content.decode('utf-8')
	with open((reports[report_id] if report_id in reports else report_id) + ".csv", "w") as file_handler:
		file_handler.write(new_report)
