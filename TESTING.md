# Manual testing using bitcoin-qt

At present the main bippy-cli binary is used to simply generate a 
hardcoded payment request to send to a pair of addresses controlled
by the `test-resources/aftok-test-recv.dat` wallet.

Using bitcoin-qt v0.13.1:

~~~{sh}

cp test-resources/aftok-test-recv.dat ~/.bitcoin/testnet3`

bitcoin-qt -testnet -rootcertificates=./test-resources/ca/certs/ca.cert.pem -wallet=aftok-test-send.dat &

stack build

.stack-work/install/x86_64-linux-nopie/lts-5.3/7.10.3/bin/bippy-cli

~~~

This will generate a bitcoin payment request file `sample1.bitcoinpaymentrequest`. 
In the bitcoin-qt console, use the menu options `File->Open URI` to open the 
payment request file. If you've provided the root certificates file correctly,
this should show an authenticated (green background) request to pay 0.0003 BTC
to 'AFTOK BIP70 Signing Service'.
