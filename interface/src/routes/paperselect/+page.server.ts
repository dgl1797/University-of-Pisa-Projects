import { env } from "$env/dynamic/private";

export async function load({ url }) {
  const host = env.PYTHON_APIS;
  const query = url.searchParams?.get("query") ?? "";
  const responseData = await (
    await fetch(`${host}/search?query=${query}&page=${0}`, {
      method: "GET",
      headers: {
        "content-type": "application/json",
      },
    })
  ).json();
  return { responseData, host, query };
}
