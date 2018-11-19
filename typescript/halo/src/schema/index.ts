type SingleParam = number | string;
type MultiParam = SingleParam[];
type Param = SingleParam | MultiParam;

interface IAPI {
    method: string;
    url: string;
    async: boolean;
}

interface IGetAPI extends IAPI {
    method: "GET";
    params?: Map<string, Param>;
}

interface IPostAPI extends IAPI {
    method: "POST";
    data?: Map<string, any>;
}

interface IPutAPI extends IAPI {
    method: "PUT";
    data?: Map<string, any>;
}

interface IAPISet {
    apis: Map<string, IAPI>;
}

export class Schema {
    public url: string;
    public apis: Map<string, IAPI|IAPISet>;
}
