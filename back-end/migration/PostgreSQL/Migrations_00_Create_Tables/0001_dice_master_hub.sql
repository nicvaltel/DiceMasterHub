BEGIN transaction;

create extension IF NOT EXISTS  citext;
create extension IF NOT EXISTS pgcrypto;

CREATE SCHEMA dice_master_hub;


create table dice_master_hub.users (
  id bigserial primary key not null,
  username text not null,
  passwd text not null,
  created timestamp with time zone default (now() at time zone 'utc')
);

create table dice_master_hub.messages (
  id bigserial primary key not null,
  user_id bigint not null,
  text text not null,
  sent timestamp with time zone default (now() at time zone 'utc'),
  FOREIGN KEY (user_id) REFERENCES dice_master_hub.users(id)
);

END TRANSACTION;