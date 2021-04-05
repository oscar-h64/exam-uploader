# AEP Exam Uploader

This program watches a given file and automatically uploads it to the specified assessment in the University of Warwick [AEP](https://altexams.warwick.ac.uk). This is useful if write on a tablet and would like your answers to be uploaded everytime you save, so if something goes wrong you have a recent copy uploaded.

## Disclaimer

**THIS PROGRAM IS NOT MADE, ENDORSED OR SUPPORTED BY THE UNIVERSITY. DO NOT CONTACT IT SERVICES RELATED TO THIS TOOL. THIS PROGRAM IS USED AT YOUR OWN RISK. FILES UPLOADED WITH THIS TOOL DO NOT BENEFIT FROM THE ENHANCED LOGGING PROVIDED BY THE AEP - IF AN UPLOAD FAILS THERE WILL LIKELY BE NO DETAILS IN THE LOG TO SHOW THE ATTEMPT. THIS IS A CONVENIENCE AND A POTENTIAL SAFETY, IT IS NOT A REPLACEMENT FOR MANUAL UPLOADS, AND IT IS NOT MY FAULT IF YOU GET 0 FOR RELYING ON IT IN ANY WAY. *YOU SHOULD ALWAYS BE PREPARED TO UPLOAD NORMALLY DURING THE 45 MINUTE UPLOAD PERIOD.***

## Usage
### Build

You must have the Haskell Stack tool installed to build this. Use
```sh
stack install --local-bin-path=path/to/save/exe/
```
to build the executable.

### Running

The executable should be run with the following 3 parameters prior to the exam:

- `--ssc SSC`: The value of the AEP `__Host-SSO-SSC-OnlineExams` cookie. You can obtain this from your browser console while on the AEP page. You may wish to delete the cookie and refresh the AEP if you have not recently been redirected via SSO, to ensure that your token will not expire during the exam.
- `--assessment UUID`: The ID of the exam to upload to. This can be found in the URL for the assessment (it will be in the form `00000000-0000-0000-0000-000000000000`).
- `--file PATH`: The path to the file to monitor and upload.

There is an optional 4th parameter, `--instance live|sandbox|URL` if you wish to select the AEP instance to use. This defaults to `live`, which is the production AEP.
