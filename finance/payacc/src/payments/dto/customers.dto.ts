import { IsString } from "class-validator";

export class CustomerInput {
  @IsString()
	email: string;

  @IsString()
	name: string;
}

export class Customer {
  id: string;
	email: string;
	name: string;
}
