import neo4j, { SessionMode } from "neo4j-driver";

const neo4jdao = (
  accessMode: SessionMode,
  port?: number,
  username?: string,
  password?: string,
  host?: string
) => {
  const dbport = port || (process.env?.NEO4J_PORT ?? 7687);
  const dbus = username || (process.env?.NEO4J_USERNAME ?? "neo4j");
  const dbpass = password || (process.env?.NEO4J_PASS ?? "password");
  const dbhost = `bolt://${host || (process.env?.NEO4J_HOST ?? "localhost")}`;

  const driver = neo4j.driver(`${dbhost}:${dbport}`, neo4j.auth.basic(`${dbus}`, `${dbpass}`));

  var session = driver.session({
    defaultAccessMode: accessMode,
  });
  return session;
};

export default neo4jdao;
