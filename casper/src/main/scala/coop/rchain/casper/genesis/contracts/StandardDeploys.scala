package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.protocol.DeployData
import coop.rchain.casper.util.ProtoUtil.stringToByteString
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.accounting

object StandardDeploys {
  private def toDeploy(
      compiledSource: CompiledRholangSource,
      user: String,
      timestamp: Long
  ): DeployData = {
    val deployData = DeployData(
      deployer = stringToByteString(user),
      timestamp = timestamp,
      term = compiledSource.code,
      phloLimit = accounting.MAX_VALUE
    )

    deployData
  }

  def listOps: DeployData = toDeploy(
    CompiledRholangSource("ListOps.rho"),
    "1d325ed35924b606264d4beaee7f78214aaecb23f6f3816055bc8bbe94280b5a",
    1539711168714L
  )
  def either: DeployData =
    toDeploy(
      CompiledRholangSource("Either.rho"),
      "89a6d9c47f360e8ce145f8fe3c773786dc86bd0e70d19643d02b0eb126473c55",
      1539794228064L
    )
  def nonNegativeNumber: DeployData =
    toDeploy(
      CompiledRholangSource("NonNegativeNumber.rho"),
      "d89a1e6d2b8f53595b3d0d47effd48f0e537d19d847ad5811cf5216157a3a63c",
      1539963224985L
    )
  def makeMint: DeployData =
    toDeploy(
      CompiledRholangSource("MakeMint.rho"),
      "d9ba2075d355755060205605f4cdbd5ecd3cce5ed1f39690f34772f7c9aa30ab",
      1539969637029L
    )
  def makePoS: DeployData =
    toDeploy(
      CompiledRholangSource("MakePoS.rho"),
      "0cbe092b27e04a944c7ac184619f3abeacffdd823ded94113ae918a63e55d5f2",
      1540221220574L
    )
  def basicWallet: DeployData =
    toDeploy(
      CompiledRholangSource("BasicWallet.rho"),
      "d72d0a7c0c9378b4874efbf871ae8089dd81f2ed3c54159fffeaba6e6fca4236",
      1540214070797L
    )
  def basicWalletFaucet: DeployData =
    toDeploy(
      CompiledRholangSource("BasicWalletFaucet.rho"),
      "7645ec68813655c4be91dc60f759804c64e5f84319d18c66e40bbfb3a202ddc8",
      1540228756441L
    )
  def walletCheck: DeployData =
    toDeploy(
      CompiledRholangSource("WalletCheck.rho"),
      "852a03854f285b36a44c7e84b1c07d30352196de60b593522653ba5e71c8e016",
      1540218618622L
    )
  def systemInstances: DeployData =
    toDeploy(
      CompiledRholangSource("SystemInstancesRegistry.rho"),
      "08a0f8fccace949453dcb6a885e9f50dd96a58ed51c49fc2e346aa4d42ffb7c1",
      1540563894858L
    )

  def lockbox: DeployData =
    toDeploy(
      CompiledRholangSource("Lockbox.rho"),
      "7e59141cbc0dfb24ca0e4fe57666895190740778871744be3cec262147e02f56",
      1552131653401L
    )

  def rev(
      wallets: Seq[PreWallet],
      faucetCode: String => String,
      posParams: ProofOfStakeParams
  ): DeployData = toDeploy(new PreWalletRev(wallets, faucetCode, posParams), "", 0L)

  def revVault: DeployData =
    toDeploy(
      CompiledRholangSource("RevVault.rho"),
      "be8ae000e4d2f29c73f792705314d71b6c0d56d7c640c6b4df9fabf90518c623",
      1551879405043L
    )

  def pos: DeployData =
    toDeploy(
      CompiledRholangSource("PoS.rho"),
      "da19138c71db4010e791f40cbf73fb1e5dda989a8f3f5bca83f324b6529c5869",
      1553702123276L
    )
}
