CREATE TABLE users (
       id SERIAL PRIMARY KEY,
       email text NOT NULL,
       name text NOT NULL,
       password text NOT NULL
);

CREATE TABLE managers (
       id SERIAL PRIMARY KEY,
       username text NOT NULL,
       name text NOT NULL,
       password text NOT NULL
);

CREATE TABLE providers (
       id SERIAL PRIMARY KEY,
       name text NOT NULL,
       slot int NOT NULL
);

CREATE TABLE queues (
       id SERIAL PRIMARY KEY,
       provider_id int REFERENCES providers(id),
       user_id int REFERENCES users(id),
       datetime TIMESTAMP WITH TIME ZONE
);
