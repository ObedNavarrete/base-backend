# Initial Configuration for API

[![Java](https://img.shields.io/badge/Java-21-blue.svg)](https://www.oracle.com/java/technologies/javase-jdk11-downloads.html)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.3.2-brightgreen.svg)](https://spring.io/projects/spring-boot)
[![PostgreSQL](https://img.shields.io/badge/PostgreSQL-14-blue.svg)](https://www.postgresql.org/download/)

## System Requirements
- Spring Boot 3.2.2
- JDK 21

## Dependencies
- spring-boot-starter-data-jpa
- spring-boot-starter-security
- spring-boot-starter-validation
- spring-boot-starter-web
- postgresql
- lombok
- spring-boot-starter-test
- spring-security-test
- jjwt-api
- jjwt-impl
- jjwt-jackson
- mapstruct
- mapstruct-processor
- flyway-core

## Public Endpoints

### Register
```http
POST /auth/register
```

#### Request
```json
{
  "name": "MyName",
  "phone": "85858585",
  "email": "email@email.com",
  "password": "123456"
}
```

#### Response
```json
{
    "success": true,
    "message": "User registered",
    "data": {
        "refreshToken": "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiI3IiwiaWF0IjoxNzA2OTcwNjYyLCJleHAiOjE3MDcxNDM0NjJ9.TKIlli3Puq0CEKXKGPNLkCAs57rVqmBseWEvSJDbAhXrQKwuWamyjc7R1UgsyZJqsnBD5M83Rw2X6fo0YYJXmQ",
        "accessToken": "eyJhbGciOiJIUzUxMiJ9.eyJuYW1lIjoiT2JlZCIsInJvbGVzIjpbIlJPTEVfVVNFUiJdLCJlbWFpbE9yUGhvbmUiOiJuZGlhem9iZWQzZ0BtYWlsLmNvbTQ1IiwiaWQiOjcsInN1YiI6IjciLCJpYXQiOjE3MDY5NzA2NjIsImV4cCI6MTcwNzA1NzA2Mn0.7oORb05DvRDaMaisgO8D7pyqqkavTyHLKIvhvPcVabaPvroYTDTAJxpgMz_nA8RsHgADOQZyysv7ljG-K3nIpg"
    }
}
```

### Login
```http
POST /auth/login
```

#### Request
```json
{
    "emailOrPhone":"email@email.com",
    "password":"123456"
}
```

#### Response
```json
{
    "success": true,
    "message": "User logged in",
    "data": {
        "refreshToken": "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiI0IiwiaWF0IjoxNzA2OTcwNzA0LCJleHAiOjE3MDcxNDM1MDR9.KAPiv1Wf0_dJ8HZz-Gxuo29ysjy73Q2kLxm0OnmrjTcU30tfgK33wqA0HSVuKfvgtFt-1AGEOHNbhxTVJ-5PQA",
        "accessToken": "eyJhbGciOiJIUzUxMiJ9.eyJuYW1lIjoiT2JlZCIsInJvbGVzIjpbIlJPTEVfVVNFUiJdLCJlbWFpbE9yUGhvbmUiOiJuZGlhem9iZWRAZ21haWwuY29tIiwiaWQiOjQsInN1YiI6IjQiLCJpYXQiOjE3MDY5NzA3MDQsImV4cCI6MTcwNzA1NzEwNH0.jXptzWyGtUI57dyC0RFRbdntS13rwEiyEPM2MTMV271jzx4kmk3Btbg9cKnyhZpF-bX8QIm10plyzNcwvUBwOw"
    }
}
```

### Refresh Token
#### Request
```http
POST /auth/refresh?request={{refreshToken}}
```

#### Response
```json
{
    "success": true,
    "message": "Token refreshed",
    "data": {
        "refreshToken": "eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiI0IiwiaWF0IjoxNzA2OTcwNzQ2LCJleHAiOjE3MDcxNDM1NDZ9.8ilYUZXtc6hw69nDSmrMQUVetg16-X9uwxtZVTOPz3ZogefAowwZxXjBCE9-UOSwl0j0SS65ZcyeoSzukCm2qA",
        "accessToken": "eyJhbGciOiJIUzUxMiJ9.eyJuYW1lIjoiT2JlZCIsInJvbGVzIjpbIlJPTEVfVVNFUiJdLCJlbWFpbE9yUGhvbmUiOiJuZGlhem9iZWRAZ21haWwuY29tIiwiaWQiOjQsInN1YiI6IjQiLCJpYXQiOjE3MDY5NzA3NDYsImV4cCI6MTcwNzA1NzE0Nn0.aBhhMwEGm5_UX3CCwu4yFQjEKSPiLiQslLbLWoHSf65GCwsmFkTCYSQ5tx6x16shGX5ocpJ7Ch09gJF6gIvdIQ"
    }
}
```

### Build Using Ant for Release
```shell
ant -f build.xml -Dapi.release.version=0.0.1-SNAPSHOT
```

### Build for get docker image
```shell
docker build --no-cache --build-arg VERSION=0.0.1-SNAPSHOT --tag security/api:0.0.1-SNAPSHOT .
```

### Save docker image on release
```shell
docker save security/api:0.0.1-SNAPSHOT | gzip > release/api-0.0.1-SNAPSHOT.tar.gz
```

### Gunzip docker image on release
```shell
gunzip -c release/api-0.0.1-SNAPSHOT.tar.gz | docker load
```
