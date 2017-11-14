CREATE SCHEMA users;

CREATE TABLE users.users (
  user_id UUID DEFAULT uuid_generate_v4() PRIMARY KEY,
  full_name TEXT DEFAULT create_user(ifull_name, iemail, ifacebook_id, ipicture_url),
  email TEXT NOT NULL,
  facebook_id TEXT UNIQUE NOT NULL,
  account_id UUID NOT NULL REFERENCES transactions.accounts (account_id),
  picture_url TEXT NOT NULL,
  created TIMESTAMP NOT NULL DEFAULT NOW()
);

CREATE FUNCTION users.create_user(
  ifull_name TEXT,
  iemail TEXT,
  ifacebook_id TEXT,
  ipicture_url TEXT
) RETURNS SETOF users.users AS $$
  begin;
  select * from users.users;
  end;
$$ LANGUAGE plpgsql;


-- CREATE FUNCTION users.create_user(
--   ifull_name TEXT,
--   iemail TEXT,
--   ifacebook_id TEXT,
--   ipicture_url TEXT
-- ) RETURNS SETOF users.users AS $$
--   BEGIN
--     RETURN QUERY
--     WITH
--       account AS (
--         SELECT * FROM transactions.create_account()
--       )
--     INSERT INTO users.users (
--       full_name,
--       email,
--       facebook_id,
--       picture_url,
--       account_id
--     ) SELECT
--       ifull_name,
--       iemail,
--       ifacebook_id,
--       ipicture_url,
--       account.account_id
--     FROM account
--       ON CONFLICT (facebook_id) DO UPDATE
--       SET full_name = EXCLUDED.full_name,
--       email = EXCLUDED.email,
--       picture_url = EXCLUDED.picture_url
--       RETURNING *;
--   END;
-- $$ LANGUAGE plpgsql;
