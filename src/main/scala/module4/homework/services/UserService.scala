package module4.homework.services

import module4.homework.dao.entity.UserId
import zio.Has
import zio.Task
import module4.homework.dao.entity.User
import module4.homework.dao.entity.Role
import module4.homework.dao.repository.UserRepository
import zio.ZIO
import zio.RIO
import module4.homework.dao.entity.UserToRole
import zio.ZLayer
import zio.macros.accessible
import module4.homework.dao.entity.RoleCode
import module4.phoneBook.db

@accessible
object UserService {
  type UserService = Has[Service]

  trait Service {
    def listUsers(): RIO[db.DataSource, List[User]]

    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]]

    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO]

    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]]
  }

  class Impl(userRepo: UserRepository.Service) extends Service {
    val dc = db.Ctx

    import dc._

    def listUsers(): RIO[db.DataSource, List[User]] =
      userRepo.list()

    def listUsersDTO(): RIO[db.DataSource, List[UserDTO]] = for {
      users <- userRepo.list()
      userDTOs <- ZIO.foreach(users) { user =>
        val roles = userRepo.userRoles(UserId(user.id))
        roles.map(r => UserDTO(user, r.toSet))
      }
    } yield userDTOs

    def addUserWithRole(user: User, roleCode: RoleCode): RIO[db.DataSource, UserDTO] = for {
      _ <- dc.transaction(
        for {
          u <- userRepo.createUser(user)
          _ <- userRepo.createRole(Role(roleCode.code, roleCode.code))
          _ <- userRepo.insertRoleToUser(roleCode, UserId(u.id))
          r <- userRepo.findRoleByCode(roleCode)
        } yield r
      )
      r <- userRepo.findRoleByCode (roleCode)
    } yield UserDTO(user, Set(r.get))

    def listUsersWithRole(roleCode: RoleCode): RIO[db.DataSource, List[UserDTO]] = for {
      usersWithRole <- userRepo.listUsersWithRole(roleCode)
      userDTOs <- ZIO.foreach(usersWithRole) { user =>
        val roles = userRepo.userRoles(UserId(user.id))
        roles.map(r => UserDTO(user, r.toSet))

      }
    } yield userDTOs
  }

  val live: ZLayer[UserRepository.UserRepository, Nothing, UserService.UserService] =
    ZLayer.fromService[UserRepository.Service, UserService.Service](repo => new Impl(repo))
}

case class UserDTO(user: User, roles: Set[Role])