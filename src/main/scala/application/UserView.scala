package application

case class UserView(firstName: String,
                    lastName: String,
                    userId: String,
                    email: String,
                    passwordHash: String,
                    verified: Boolean)
