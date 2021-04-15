create table if not exists users
(
    id      serial primary key,
    name    varchar(64),
    surname varchar(64),
    email   varchar(64)
);

create table if not exists terms
(
    id   serial primary key,
    text varchar(200)
);

create table if not exists rules
(
    id   serial primary key,
    text varchar(200)
);

create table if not exists periods
(
    id             serial primary key,
    datetime_start varchar(64),
    datetime_end   varchar(64)
);

create table if not exists service
(
    id        serial primary key,
    name      varchar(64),
    author_id int references users (id),
    version   int,
    term_id   int references terms,
    rule_id   int references rules,
    period_id int references periods
);

create table if not exists subscription
(
    user_id    int references users (id),
    service_id int references service (id)
);

