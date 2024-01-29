create sequence _user_id_seq
    start with 1
    increment by 1
    no minvalue
    no maxvalue
    cache 1;

create table _user
(
    id            integer default nextval('_user_id_seq'::regclass) not null
        constraint _user_pkey
            primary key,
    email         varchar(50) unique not null,
    enabled       boolean not null default true,
    name          varchar(50) not null,
    password      varchar(100) not null,
    phone         varchar(20) not null unique,
    created_by    integer null
        constraint fk4qbg83cirguwh8rkuyrhdb0af
            references _user,
    updated_by    integer null
        constraint fkr33f4vi8pg6p0xxv4nyevuv5k
            references _user,
    created_at    timestamp not null default CURRENT_TIMESTAMP,
    updated_at    timestamp,
    created_by_ip varchar(20),
    updated_by_ip varchar (20),
    passive       boolean not null default false
);

create sequence role_id_seq
    start with 1
    increment by 1
    no minvalue
    no maxvalue
    cache 1;

create table role
(
    id          integer default nextval('role_id_seq'::regclass) not null
        constraint role_pkey
            primary key,
    description varchar(20),
    name        varchar(20)
);

create table user_role
(
    id_user integer not null
        constraint fkdynwvl1dt9s7kallgcqwwih1f
            references _user,
    id_role integer not null
        constraint fk2aam9nt2tv8vcfymi3jo9c314
            references role,
    primary key (id_user, id_role)
);

INSERT INTO role (id, description, name) VALUES (1, 'Admin', 'ROLE_ADMIN');
INSERT INTO role (id, description, name) VALUES (2, 'User', 'ROLE_USER');