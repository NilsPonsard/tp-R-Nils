const key = "<YOUR-API-KEY-HERE>";

const username = "sautax";

async function fetchRiotApi(url: string) {
    const response = await fetch(url, {
        headers: {
            "X-Riot-Token": key,
        },
    });
    return await response.json();
}

const summoner = await fetchRiotApi(
    "https://euw1.api.riotgames.com/lol/summoner/v4/summoners/by-name/" +
        username
);

const puuid = summoner.puuid;

if (typeof puuid === "undefined") {
    throw new Error("puuid is undefined");
}

const matchList: Array<string> = [];

const matchesPerRequest = 100;

let current = 0;

while (true) {
    const currentMatchList = await fetchRiotApi(
        `https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/${puuid}/ids?start=${current}&count=${matchesPerRequest}`
    );
    console.log(currentMatchList);
    matchList.push(...currentMatchList);
    if (currentMatchList.length === 0) {
        break;
    }
    current += currentMatchList.length;
}

const matchDetails: Array<any> = [];

for (const match of matchList) {
    let valid = false;

    let currentMatchDetails;
    while (!valid) {
        currentMatchDetails = await fetchRiotApi(
            `https://europe.api.riotgames.com/lol/match/v5/matches/${match}`
        );

        console.log(currentMatchDetails);
        if (currentMatchDetails?.status?.status_code === 429) {
            console.log("error 429, waiting 10 sec");
            await new Promise((resolve) => setTimeout(resolve, 10000));
        } else {
            valid = true;
        }
    }
    matchDetails.push(currentMatchDetails);
}

Deno.writeTextFileSync("matchDetails.json", JSON.stringify(matchDetails));
