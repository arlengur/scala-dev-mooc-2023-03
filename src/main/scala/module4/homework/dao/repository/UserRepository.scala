package module4.homework.dao.repository

import io.getquill.Ord
import zio.{Has, ULayer, ZIO, ZLayer}
import io.getquill.context.ZioJdbc._
import module4.homework.dao.entity.User
import zio.macros.accessible
import module4.homework.dao.entity.{Role, UserToRole}
import module4.homework.dao.entity.UserId
import module4.homework.dao.entity.RoleCode
import module4.phoneBook.db

import java.sql.SQLException
import javax.sql.DataSource


object UserRepository {


  val dc = db.Ctx

  import dc._

  type UserRepository = Has[Service]

  trait Service {
    def findUser(userId: UserId): QIO[Option[User]]

    def createUser(user: User): QIO[User]

    def createRole(role: Role): QIO[Role]

    def createUsers(users: List[User]): QIO[List[User]]

    def updateUser(user: User): QIO[Unit]

    def deleteUser(user: User): QIO[Unit]

    def findByLastName(lastName: String): QIO[List[User]]

    def list(): QIO[List[User]]

    def userRoles(userId: UserId): QIO[List[Role]]

    def insertRoleToUser(roleCode: RoleCode, userId: UserId): QIO[Unit]

    def listUsersWithRole(roleCode: RoleCode): QIO[List[User]]

    def findRoleByCode(roleCode: RoleCode): QIO[Option[Role]]
  }

  class ServiceImpl extends Service {

    lazy val userSchema = quote {
      querySchema[User](""""User"""")
    }


    lazy val roleSchema = quote {
      querySchema[Role](""""Role"""")
    }

    lazy val userToRoleSchema = quote {
      querySchema[UserToRole](""""UserToRole"""")
    }

    def findUser(userId: UserId): Result[Option[User]] = run(userSchema.filter(_.id == lift(userId.id)))
      .map(_.headOption)

    def createUser(user: User): Result[User] = run(userSchema.insert(lift(user))).as(user)

    def createRole(role: Role): Result[Role] = run(roleSchema.insert(lift(role))).as(role)

    def createUsers(users: List[User]): Result[List[User]] = run(
      quote {
        liftQuery(users).foreach(u => userSchema.insert(u))
      }
    ).as(users)

    def updateUser(user: User): Result[Unit] = run(userSchema.filter(_.id == lift(user.id)).update(lift(user))).as(())

    def deleteUser(user: User): Result[Unit] = run(userSchema.filter(_.id == lift(user.id)).delete).as(())

    def findByLastName(lastName: String): Result[List[User]] = run(userSchema.filter(_.lastName == lift(lastName)))

    def list(): Result[List[User]] = run(userSchema)

    def userRoles(userId: UserId): Result[List[Role]] = run(roleSchema)

    def insertRoleToUser(roleCode: RoleCode, userId: UserId): Result[Unit] = run(
      userToRoleSchema.insert(lift(UserToRole(roleCode.code, userId.id)))
    ).as(())

    def listUsersWithRole(roleCode: RoleCode): Result[List[User]] = run(
      for {
        usersIdsWithRole <- userToRoleSchema.filter(_.roleId == lift(roleCode.code)).map(_.userId)
        usersWithRole <- userSchema.filter(_.id == usersIdsWithRole)
      } yield usersWithRole
    )

    def findRoleByCode(roleCode: RoleCode): Result[Option[Role]] =
      run(roleSchema.filter(_.code == lift(roleCode.code)).sortBy(_.name)(Ord.asc).take(1)).map(_.headOption)
  }

  val live: ULayer[UserRepository] = ZLayer.succeed(new ServiceImpl())
}