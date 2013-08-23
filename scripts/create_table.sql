DROP TABLE queues;
DROP TABLE providers;
DROP TABLE managers;
DROP TABLE users;

CREATE TABLE users (
       id SERIAL PRIMARY KEY,
       email text NOT NULL,
       name text NOT NULL,
       password text NOT NULL
);
INSERT INTO users (email, name, password) VALUES
       ('js@world.com', 'Jie Sheng', '7c211433f02071597741e6ff5a8ea34789abbf43'),
       ('js@hello.com', 'Jason', 'aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d');

CREATE TABLE managers (
       id SERIAL PRIMARY KEY,
       username text NOT NULL,
       name text NOT NULL,
       password text NOT NULL
);

CREATE TABLE providers (
       id SERIAL PRIMARY KEY,
       name text NOT NULL
);

INSERT INTO providers (name) VALUES
       ('Chams Clinic'),
       ('Silver Cross Clinic');

CREATE TABLE queues (
       id SERIAL PRIMARY KEY,
       provider_id int REFERENCES providers(id),
       user_id int REFERENCES users(id),
       datetime TIMESTAMP WITH TIME ZONE,
       datetime_arrived TIMESTAMP WITH TIME ZONE NULL,
       datetime_called TIMESTAMP WITH TIME ZONE NULL
);
