# Test Certificate Chain Generation Process
# This closely follows the process described at 
# https://jamielinux.com/docs/openssl-certificate-authority/index.html

# Generate the private key for the CA
openssl genrsa -aes256 -out private/ca.key.pem 4096
# password test_ca_secret

# Create the root certificate
openssl req -config ca.cnf \
      -key private/ca.key.pem \
      -new -x509 -days 7300 -sha256 -extensions v3_ca \
      -out certs/ca.cert.pem

# Verify the root certificate
openssl x509 -noout -text -in certs/ca.cert.pem

# Generate the intermediate CA key
openssl genrsa -aes256 -out intermediate/private/intermediate.key.pem 4096
# password test_intermediate_secret

# Create the intermediate certificate
openssl req -config intermediate.cnf -new -sha256 \
      -key intermediate/private/intermediate.key.pem \
      -out intermediate/csr/intermediate.csr.pem

# Sign the intermediate certificate with the root CA's key
openssl ca -config ca.cnf -extensions v3_intermediate_ca \
      -days 3650 -notext -md sha256 \
      -in intermediate/csr/intermediate.csr.pem \
      -out intermediate/certs/intermediate.cert.pem

# Verify the intermediate cert
openssl x509 -noout -text -in intermediate/certs/intermediate.cert.pem
openssl verify -CAfile certs/ca.cert.pem intermediate/certs/intermediate.cert.pem

# Create the intermediate certificate chain file
cat intermediate/certs/intermediate.cert.pem \
      certs/ca.cert.pem > intermediate/certs/ca-chain.cert.pem
chmod 444 intermediate/certs/ca-chain.cert.pem

# Generate the signing key for the payment requests
openssl genrsa \
      -out intermediate/private/aftok.bip70.key.pem 2048

chmod 400 intermediate/private/aftok.bip70.key.pem

# Generate the CSR for the client certificate
openssl req -config intermediate.cnf \
      -key intermediate/private/aftok.bip70.key.pem \
      -new -sha256 -out intermediate/csr/aftok.bip70.csr.pem

# Use the intermediate cert to sign the client certificate
openssl ca -config intermediate.cnf \
      -extensions server_cert -days 375 -notext -md sha256 \
      -in intermediate/csr/aftok.bip70.csr.pem \
      -out intermediate/certs/aftok.bip70.cert.pem

chmod 444 intermediate/certs/aftok.bip70.cert.pem

# Verify the client certificate
openssl x509 -noout -text \
      -in intermediate/certs/aftok.bip70.cert.pem

# Verify using certificate chain
openssl verify -CAfile intermediate/certs/ca-chain.cert.pem \
      intermediate/certs/aftok.bip70.cert.pem

#openssl x509 -trustout -signkey ca.key -days 365 -req -in ca.csr -out ca.pem
##Certificate A is created like this:
#
#openssl genrsa -out client.key 1024
#openssl req -new -key client.key -out client.csr
#openssl ca -in client.csr -out client.cer
##This command implicitly depends on the root certificate, for which it finds the required info in the openssl configuration file.
#
##Certificate B however must only rely on A, which is not registered in the config file, so the previous command won't work here.
#
##I found the answer in this article. Certificate B (chain A -> B) can be created with these two commands:
#
## Create a certificate request
#openssl req -new -keyout B.key -out B.request -days 365
#
## Create and sign the certificate
#openssl ca -policy policy_anything -keyfile A.key -cert A.pem -out B.pem -infiles B.request
