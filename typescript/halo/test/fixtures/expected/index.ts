import HaloClientUsers from "./users";

export class HaloClient {
    private url: string;

    public constructor(url: string) {
        this.url = url;
    }

    public get users(): HaloClientUsers {
        return new HaloClientUsers(`${this.url}/users`);
    }
}
